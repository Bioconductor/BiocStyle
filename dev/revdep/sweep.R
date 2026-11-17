## In-container orchestrator for the BiocStyle reverse-dependency sweep.
##
## Usage: Rscript /revdep/sweep.R <run_dir>
##
## <run_dir> was prepared by run.sh: config.tsv, forced.txt and the two
## BiocStyle source snapshots in biocstyle/{baseline,candidate}. Every step
## writes its result under <run_dir> and is skipped when that result
## already exists, so re-running on the same directory resumes the sweep.
args <- commandArgs(trailingOnly = TRUE)
stopifnot(length(args) == 1L)
run_dir <- normalizePath(args[[1]])
script_dir <- "/revdep"
shared_lib <- "/revdep-cache/lib"
variants <- c("baseline", "candidate")

dir.create(shared_lib, recursive = TRUE, showWarnings = FALSE)
.libPaths(c(shared_lib, .libPaths()))
options(Ncpus = parallel::detectCores(), warn = 1)

cfg_tab <- read.delim(file.path(run_dir, "config.tsv"), colClasses = "character")
cfg <- setNames(as.list(cfg_tab$value), cfg_tab$key)
vignette_timeout <- as.integer(cfg$timeout)
jobs <- max(1L, min(2L, as.integer(cfg$jobs)))
bioc_repo <- BiocManager::repositories()[["BioCsoft"]]

timings_file <- file.path(run_dir, "timings.tsv")
record_timing <- function(phase, package, step, seconds) {
    cat(sprintf("%s\t%s\t%s\t%.1f\n", phase, package, step, seconds),
        file = timings_file, append = TRUE)
}
timed <- function(expr) {
    t0 <- Sys.time()
    value <- expr
    list(value = value, seconds = as.numeric(difftime(Sys.time(), t0, units = "secs")))
}
log_line <- function(...) message(format(Sys.time(), "[%H:%M:%S] "), ...)

## Run a shell command, sending stdout+stderr to `log`; return exit status.
run_logged <- function(cmd, log, env = character()) {
    full <- paste(c(env, cmd, ">", shQuote(log), "2>&1"), collapse = " ")
    system(full)
}

## ---- BiocStyle variants ----------------------------------------------------
install_biocstyle_variant <- function(variant) {
    vlib <- file.path(run_dir, "lib", variant)
    if (file.exists(file.path(vlib, "BiocStyle", "DESCRIPTION"))) return(invisible())
    dir.create(vlib, recursive = TRUE, showWarnings = FALSE)
    dir.create(file.path(run_dir, "logs"), showWarnings = FALSE)
    src <- file.path(run_dir, "biocstyle", variant)
    imports <- read.dcf(file.path(src, "DESCRIPTION"), fields = "Imports")[1, 1]
    imports <- sub("[ (].*$", "", trimws(strsplit(gsub("\\s+", " ", imports), ",")[[1]]))
    missing <- setdiff(imports, c(rownames(installed.packages()), "R"))
    if (length(missing))
        BiocManager::install(missing, lib = shared_lib, update = FALSE, ask = FALSE)
    log <- file.path(run_dir, "logs", sprintf("biocstyle-%s.log", variant))
    status <- run_logged(c("R", "CMD", "INSTALL", "--no-test-load",
                           paste0("--library=", shQuote(vlib)), shQuote(src)), log,
                         sprintf("R_LIBS=%s", shQuote(paste(vlib, shared_lib, sep = ":"))))
    if (status != 0) stop("installing the ", variant, " BiocStyle failed; see ", log)
}

for (v in variants) {
    t <- timed(install_biocstyle_variant(v))
    record_timing("setup", "-", paste0("biocstyle_", v), t$seconds)
}
log_line("BiocStyle baseline ",
         as.character(packageVersion("BiocStyle", lib.loc = file.path(run_dir, "lib", "baseline"))),
         ", candidate ",
         as.character(packageVersion("BiocStyle", lib.loc = file.path(run_dir, "lib", "candidate"))))

## ---- Package selection -----------------------------------------------------
read_forced <- function(path) {
    if (!file.exists(path)) return(character())
    x <- trimws(sub("#.*$", "", readLines(path, warn = FALSE)))
    unique(x[nzchar(x)])
}

select_packages <- function() {
    db <- available.packages(repos = bioc_repo, type = "source")
    revdeps <- tools::package_dependencies(
        "BiocStyle", db = db, reverse = TRUE,
        which = c("Depends", "Imports", "Suggests"))[["BiocStyle"]]
    scores <- read.delim("https://bioconductor.org/packages/stats/bioc/bioc_pkg_scores.tab")
    scores <- scores[scores$Package %in% revdeps, ]
    scores <- scores[order(-scores$Download_score, scores$Package), ]
    scores$rank <- seq_len(nrow(scores))

    requested <- if (nzchar(cfg$pkgs)) trimws(strsplit(cfg$pkgs, ",")[[1]]) else character()
    if (length(requested)) {
        pkgs <- requested
        reason <- rep("requested", length(pkgs))
    } else {
        top <- head(scores$Package, as.integer(cfg$n))
        forced <- read_forced(file.path(run_dir, "forced.txt"))
        pkgs <- union(top, forced)
        reason <- ifelse(pkgs %in% top & pkgs %in% forced, "top-n,forced",
                         ifelse(pkgs %in% top, "top-n", "forced"))
    }
    reason[!pkgs %in% revdeps] <- paste(reason[!pkgs %in% revdeps], "(not a BiocStyle reverse dependency)")
    available <- pkgs %in% rownames(db)
    reason[!available] <- paste(reason[!available], "(not in Bioc devel; skipped)")
    data.frame(package = pkgs, reason = reason,
               download_score = scores$Download_score[match(pkgs, scores$Package)],
               rank = scores$rank[match(pkgs, scores$Package)],
               available = available,
               n_revdeps = length(revdeps))
}

sample_file <- file.path(run_dir, "sample.tsv")
if (!file.exists(sample_file)) {
    t <- timed(select_packages())
    write.table(t$value, sample_file, sep = "\t", quote = FALSE, row.names = FALSE, na = "")
    record_timing("setup", "-", "select_packages", t$seconds)
}
sample <- read.delim(sample_file, colClasses = c(package = "character"))
log_line("Sample: ", nrow(sample), " packages: ", paste(sample$package, collapse = ", "))

## ---- Per-package sweep -------------------------------------------------------
render_variant <- function(variant, pkg, src_dir, vignettes, pkg_dir) {
    out_dir <- file.path(pkg_dir, variant)
    dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
    todo <- vignettes[!file.exists(file.path(out_dir, paste0(basename(vignettes), ".json")))]
    if (!length(todo)) return(0)
    ## A fresh copy per variant, on the container's own filesystem: the
    ## macOS bind mount breaks HDF5 file locking and chmod in some vignettes.
    work <- file.path("/tmp/revdep-work", pkg, variant)
    unlink(work, recursive = TRUE)
    dir.create(dirname(work), recursive = TRUE, showWarnings = FALSE)
    system2("cp", c("-a", shQuote(src_dir), shQuote(work)))
    on.exit(unlink(work, recursive = TRUE))
    env <- sprintf("R_LIBS=%s", shQuote(paste(file.path(run_dir, "lib", variant), shared_lib, sep = ":")))
    t0 <- Sys.time()
    for (vig in todo) {
        status_json <- file.path(out_dir, paste0(basename(vig), ".json"))
        log <- file.path(out_dir, paste0(basename(vig), ".log"))
        started <- Sys.time()
        code <- run_logged(c("timeout", "--kill-after=30", vignette_timeout,
                             "Rscript", file.path(script_dir, "render-one.R"),
                             shQuote(work), shQuote(vig), shQuote(out_dir), shQuote(status_json)),
                           log, env)
        if (!file.exists(status_json)) {
            timed_out <- code %in% c(124L, 137L)
            tail_lines <- tail(readLines(log, warn = FALSE), 12L)
            jsonlite::write_json(list(
                status = if (timed_out) "timeout" else "error",
                output = NA_character_,
                error = if (timed_out) sprintf("timed out after %s s", vignette_timeout)
                        else c(sprintf("render process exited with status %d", code), tail_lines),
                elapsed = round(as.numeric(difftime(Sys.time(), started, units = "secs")), 1)
            ), status_json, auto_unbox = TRUE, pretty = TRUE)
        }
        st <- jsonlite::read_json(status_json)
        log_line(sprintf("  %s %-9s %-40s %s (%.0fs)", pkg, variant, basename(vig),
                         st$status, st$elapsed))
    }
    as.numeric(difftime(Sys.time(), t0, units = "secs"))
}

write_package_status <- function(pkg_dir, status, ...) {
    jsonlite::write_json(list(status = status, ...), file.path(pkg_dir, "package.json"),
                         auto_unbox = TRUE, pretty = TRUE)
    file.create(file.path(pkg_dir, "done"))
}

process_package <- function(pkg) {
    pkg_dir <- file.path(run_dir, "pkgs", pkg)
    dir.create(file.path(pkg_dir, "src"), recursive = TRUE, showWarnings = FALSE)
    log_line(pkg, ": downloading")
    tarball <- list.files(file.path(pkg_dir, "src"), pattern = "\\.tar\\.gz$", full.names = TRUE)
    if (!length(tarball)) {
        t <- timed(tryCatch(
            download.packages(pkg, file.path(pkg_dir, "src"), repos = bioc_repo,
                              type = "source", quiet = TRUE)[, 2],
            error = function(e) character()))
        record_timing("package", pkg, "download", t$seconds)
        tarball <- t$value
    }
    if (!length(tarball)) return(write_package_status(pkg_dir, "download-failed"))
    src_dir <- file.path(pkg_dir, "src", pkg)
    if (!dir.exists(src_dir)) untar(tarball, exdir = file.path(pkg_dir, "src"))

    log_line(pkg, ": installing dependencies")
    install_log <- file.path(pkg_dir, "install.log")
    install_json <- file.path(pkg_dir, "install.json")
    t <- timed(run_logged(c("Rscript", file.path(script_dir, "install-pkg.R"),
                            shQuote(src_dir), shQuote(tarball), shQuote(shared_lib),
                            shQuote(install_json)),
                          install_log, sprintf("R_LIBS=%s", shared_lib)))
    record_timing("package", pkg, "install", t$seconds)
    install <- if (file.exists(install_json)) jsonlite::read_json(install_json, simplifyVector = TRUE)
               else list(package_installed = FALSE, missing_dependencies = character())
    if (!isTRUE(install$package_installed))
        return(write_package_status(pkg_dir, "install-failed",
                                    missing_dependencies = install$missing_dependencies))

    vinfo <- tools::pkgVignettes(dir = src_dir, check = FALSE)
    docs <- if (is.null(vinfo)) character() else vinfo$docs
    vignettes <- substring(normalizePath(docs), nchar(normalizePath(src_dir)) + 2L)
    if (!length(vignettes))
        return(write_package_status(pkg_dir, "no-vignettes",
                                    missing_dependencies = install$missing_dependencies))
    uses_biocstyle <- vapply(docs, function(f)
        any(grepl("BiocStyle", readLines(f, warn = FALSE), fixed = TRUE)), logical(1))
    jsonlite::write_json(data.frame(vignette = basename(vignettes), path = vignettes,
                                    uses_biocstyle = unname(uses_biocstyle)),
                         file.path(pkg_dir, "vignettes.json"), pretty = TRUE)

    log_line(pkg, ": rendering ", length(vignettes), " vignette(s) x 2 variants")
    secs <- parallel::mclapply(variants, render_variant, pkg = pkg, src_dir = src_dir,
                               vignettes = vignettes, pkg_dir = pkg_dir,
                               mc.cores = jobs, mc.preschedule = FALSE)
    for (i in seq_along(variants)) {
        if (inherits(secs[[i]], "try-error")) stop("rendering ", pkg, " failed: ", secs[[i]])
        record_timing("package", pkg, paste0("render_", variants[[i]]), secs[[i]])
    }
    write_package_status(pkg_dir, "rendered",
                         missing_dependencies = install$missing_dependencies)
}

for (pkg in sample$package[sample$available]) {
    if (file.exists(file.path(run_dir, "pkgs", pkg, "done"))) {
        log_line(pkg, ": already done, skipping")
        next
    }
    t <- timed(process_package(pkg))
    record_timing("package", pkg, "total", t$seconds)
}

## ---- Compare and report ----------------------------------------------------------
system2("Rscript", c(file.path(script_dir, "compare.R"), shQuote(run_dir)))
