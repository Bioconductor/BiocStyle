## Render one vignette with tools::buildVignette(), the same entry point
## R CMD build uses. Run in a fresh R process (under `timeout`) by sweep.R,
## with R_LIBS pointing at the variant's BiocStyle library first.
##
## Usage: Rscript render-one.R <pkg_src_dir> <vignette_file> <out_dir> <status_json>
##
## Writes <status_json> with status "ok" or "error". A timeout or crash
## leaves no status file; sweep.R records those.
args <- commandArgs(trailingOnly = TRUE)
stopifnot(length(args) == 4L)
src_dir <- args[[1]]
vignette_file <- args[[2]]
out_dir <- args[[3]]
status_json <- args[[4]]

started <- Sys.time()
vig_dir <- dirname(file.path(src_dir, vignette_file))
## Load the VignetteBuilder packages so their engines are registered, but
## don't pass them as buildVignette(buildPkg=): that restricts engine lookup
## to those packages and hides utils::Sweave.
vb <- read.dcf(file.path(src_dir, "DESCRIPTION"), fields = "VignetteBuilder")[1, 1]
if (!is.na(vb))
    for (b in trimws(strsplit(vb, ",")[[1]])) suppressPackageStartupMessages(loadNamespace(b))

biocstyle_path <- tryCatch(find.package("BiocStyle"), error = function(e) NA_character_)
biocstyle_version <- tryCatch(as.character(packageVersion("BiocStyle")),
                              error = function(e) NA_character_)

render <- function() {
    ## Like R CMD build: the package's own namespace is available and the
    ## working directory is the vignettes directory.
    tools::buildVignette(file.path(vig_dir, basename(vignette_file)), dir = vig_dir,
                         tangle = FALSE, quiet = FALSE, clean = TRUE)
}

## A missing LaTeX package is an environment gap, not a BiocStyle effect:
## install it with tinytex and retry once (applies equally to both variants).
missing_latex <- function(msg) {
    grepl("! LaTeX Error: File `[^']+' not found|! Font [^ ]+ not loadable|I can't find file", msg)
}

attempt <- function() tryCatch(list(output = render(), error = NULL),
                                error = function(e) list(output = NULL, error = conditionMessage(e)))
res <- attempt()
latex_retry <- FALSE
if (!is.null(res$error) && missing_latex(res$error)) {
    logs <- list.files(vig_dir, pattern = "\\.log$", full.names = TRUE)
    msg_file <- tempfile(fileext = ".log")
    writeLines(c(res$error, unlist(lapply(logs, readLines, warn = FALSE))), msg_file)
    system(sprintf("flock /tmp/tlmgr.lock Rscript -e %s",
                   shQuote(sprintf("tinytex::parse_install(%s)", deparse(msg_file)))))
    latex_retry <- TRUE
    res <- attempt()
}

output <- NA_character_
if (is.null(res$error)) {
    produced <- file.path(vig_dir, res$output)
    produced <- produced[file.exists(produced)]
    if (length(produced)) {
        dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
        file.copy(produced[[1]], out_dir, overwrite = TRUE)
        output <- basename(produced[[1]])
    } else {
        res$error <- "buildVignette() returned but no output file was found"
    }
}

error_lines <- if (is.null(res$error)) character() else
    head(strsplit(res$error, "\n", fixed = TRUE)[[1]], 12L)

jsonlite::write_json(list(
    status = if (is.null(res$error)) "ok" else "error",
    output = output,
    error = error_lines,
    elapsed = round(as.numeric(difftime(Sys.time(), started, units = "secs")), 1),
    latex_retry = latex_retry,
    biocstyle_path = biocstyle_path,
    biocstyle_version = biocstyle_version
), status_json, auto_unbox = TRUE, pretty = TRUE)
