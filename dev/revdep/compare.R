## Compare baseline vs candidate vignette renders and write report.md.
##
## Usage: Rscript /revdep/compare.R <run_dir>
##
## HTML: normalize away render-to-render noise, then `diff -u`.
## PDF:  compare page counts (pdfinfo) and normalized `pdftotext -layout`.
## Normalized copies and diffs go to <run_dir>/compare/<pkg>/.
args <- commandArgs(trailingOnly = TRUE)
stopifnot(length(args) == 1L)
run_dir <- normalizePath(args[[1]])
variants <- c("baseline", "candidate")

## ---- Normalization ------------------------------------------------------------
## Replace each base64 data: URI payload. Text payloads (CSS, JS, SVG) are
## decoded inline so style changes show as readable diff lines; binary
## payloads (images, fonts) become a short content hash.
normalize_data_uris <- function(txt) {
    pattern <- "data:([A-Za-z0-9.+/-]+)((;[A-Za-z0-9=._-]+)*);base64,([A-Za-z0-9+/=]+)"
    m <- gregexpr(pattern, txt, perl = TRUE)
    uris <- regmatches(txt, m)[[1]]
    if (!length(uris)) return(txt)
    replacement <- vapply(uris, function(u) {
        mime <- sub("^data:([^;,]+).*$", "\\1", u)
        payload <- sub("^.*;base64,", "", u)
        if (grepl("^(text/|application/(javascript|json|ecmascript)|image/svg)", mime)) {
            decoded <- tryCatch(rawToChar(jsonlite::base64_dec(payload)), error = function(e) NA)
            if (!is.na(decoded) && validUTF8(decoded))
                return(paste0("data:", mime, ";base64-decoded,\n", decoded, "\n"))
        }
        ## upper case so the hex-id rule below does not erase it
        paste0("data:", mime, ";base64,MD5:",
               toupper(digest::digest(payload, "md5", serialize = FALSE)))
    }, character(1))
    regmatches(txt, m) <- list(replacement)
    txt
}

## Substitutions applied to both HTML and PDF text, in order.
## Each removes a source of difference between two renders of the same
## vignette that is unrelated to BiocStyle.
noise_rules <- list(
    ## per-variant working copy and library paths
    c("/run-dir/pkgs/[^/]+/work-(baseline|candidate)", "<WORKDIR>"),
    c("/run-dir/lib/(baseline|candidate)", "<BIOCSTYLE_LIB>"),
    c("/tmp/Rtmp[A-Za-z0-9]+", "/tmp/RtmpXXXXXX"),
    ## the BiocStyle version itself (shown by sessionInfo())
    c("BiocStyle(_| +)[0-9]+\\.[0-9]+\\.[0-9]+", "BiocStyle\\1<VERSION>"),
    ## timestamps and dates
    c("[0-9]{4}-[0-9]{2}-[0-9]{2}[ T][0-9]{2}:[0-9]{2}(:[0-9]{2}(\\.[0-9]+)?)?( ?[A-Z]{2,5}|[+-][0-9]{2}:?[0-9]{2}|Z)?", "<DATETIME>"),
    c("[0-9]{1,2}:[0-9]{2}:[0-9]{2}( ?[AP]M)?( [A-Z]{2,5})?", "<TIME>"),
    c("[0-9]{4}-[0-9]{2}-[0-9]{2}", "<DATE>"),
    c("[0-9]{1,2} (January|February|March|April|May|June|July|August|September|October|November|December),? [0-9]{4}", "<DATE>"),
    c("(January|February|March|April|May|June|July|August|September|October|November|December) [0-9]{1,2},? [0-9]{4}", "<DATE>"),
    ## random element ids (htmlwidgets, plotly, DT, pandoc footnote/TOC ids)
    c("htmlwidget-[0-9a-f]{8,}", "htmlwidget-<ID>"),
    c("\\b[0-9a-f]{20,}\\b", "<HEXID>")
)

apply_rules <- function(txt) {
    for (r in noise_rules) txt <- gsub(r[[1]], r[[2]], txt, perl = TRUE)
    txt
}

normalize_html <- function(path) {
    txt <- paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
    txt <- normalize_data_uris(txt)
    ## one tag per line, so a change shows as a few lines rather than one
    ## very long one
    txt <- gsub(">[ \t]*<", ">\n<", txt, perl = TRUE)
    apply_rules(strsplit(txt, "\n", fixed = TRUE)[[1]])
}

pdf_pages <- function(path) {
    info <- system2("pdfinfo", shQuote(path), stdout = TRUE, stderr = FALSE)
    as.integer(sub("^Pages:\\s+", "", grep("^Pages:", info, value = TRUE)))
}

normalize_pdf_text <- function(path) {
    txt <- system2("pdftotext", c("-layout", shQuote(path), "-"), stdout = TRUE, stderr = FALSE)
    apply_rules(sub("\\s+$", "", txt))
}

## Unified diff of two character vectors; returns number of changed lines.
diff_lines <- function(a, b, out_prefix) {
    fa <- paste0(out_prefix, ".baseline.txt")
    fb <- paste0(out_prefix, ".candidate.txt")
    writeLines(a, fa)
    writeLines(b, fb)
    d <- suppressWarnings(system2("diff", c("-u", shQuote(fa), shQuote(fb)), stdout = TRUE))
    if (length(d)) writeLines(d, paste0(out_prefix, ".diff")) else unlink(paste0(out_prefix, ".diff"))
    sum(grepl("^[+-]", d) & !grepl("^(\\+\\+\\+|---) ", d))
}

## ---- Collect results ------------------------------------------------------------
read_json_or_null <- function(path) if (file.exists(path)) jsonlite::read_json(path, simplifyVector = TRUE)

sample <- read.delim(file.path(run_dir, "sample.tsv"), colClasses = c(package = "character"))
cfg_tab <- read.delim(file.path(run_dir, "config.tsv"), colClasses = "character")
cfg <- setNames(as.list(cfg_tab$value), cfg_tab$key)

rows <- list()
pkg_problems <- list()
for (pkg in sample$package) {
    pkg_dir <- file.path(run_dir, "pkgs", pkg)
    pstat <- read_json_or_null(file.path(pkg_dir, "package.json"))
    if (is.null(pstat)) {
        pkg_problems[[pkg]] <- if (isTRUE(sample$available[sample$package == pkg]))
            "not processed yet" else "not available in Bioc devel"
        next
    }
    if (length(pstat$missing_dependencies))
        pkg_problems[[pkg]] <- paste("missing dependencies:", paste(pstat$missing_dependencies, collapse = ", "))
    if (pstat$status != "rendered") {
        pkg_problems[[pkg]] <- paste(c(pstat$status, pkg_problems[[pkg]]), collapse = "; ")
        next
    }
    vigs <- read_json_or_null(file.path(pkg_dir, "vignettes.json"))
    cmp_dir <- file.path(run_dir, "compare", pkg)
    dir.create(cmp_dir, recursive = TRUE, showWarnings = FALSE)
    for (i in seq_len(nrow(vigs))) {
        vig <- vigs$vignette[[i]]
        st <- lapply(setNames(variants, variants), function(v)
            read_json_or_null(file.path(pkg_dir, v, paste0(vig, ".json"))))
        status <- vapply(st, function(s) if (is.null(s)) "missing" else s$status, character(1))
        row <- list(package = pkg, vignette = vig, uses_biocstyle = vigs$uses_biocstyle[[i]],
                    baseline = status[["baseline"]], candidate = status[["candidate"]],
                    html_diff = NA_integer_, pdf_pages = NA_character_, pdf_delta = NA_integer_,
                    pdf_text_diff = NA_integer_, diff_file = NA_character_,
                    baseline_error = paste(st$baseline$error, collapse = "\n"),
                    candidate_error = paste(st$candidate$error, collapse = "\n"),
                    biocstyle_check = NA_character_)
        ## Each render must have loaded its own variant's BiocStyle.
        bad <- vapply(variants, function(v) {
            p <- st[[v]]$biocstyle_path
            !is.null(p) && !identical(p, file.path("/run-dir/lib", v, "BiocStyle"))
        }, logical(1))
        if (any(bad)) row$biocstyle_check <- paste(
            vapply(variants[bad], function(v) paste0(v, " loaded ", st[[v]]$biocstyle_path), ""),
            collapse = "; ")
        if (all(status == "ok")) {
            outs <- vapply(variants, function(v) file.path(pkg_dir, v, st[[v]]$output), "")
            prefix <- file.path(cmp_dir, sub("\\.[^.]+$", "", vig))
            if (all(grepl("\\.html?$", outs))) {
                row$html_diff <- diff_lines(normalize_html(outs[[1]]), normalize_html(outs[[2]]),
                                            paste0(prefix, ".html"))
                if (row$html_diff > 0) row$diff_file <- paste0(prefix, ".html.diff")
            } else if (all(grepl("\\.pdf$", outs))) {
                pages <- vapply(outs, pdf_pages, integer(1))
                row$pdf_pages <- paste(pages, collapse = "→")
                row$pdf_delta <- pages[[2]] - pages[[1]]
                row$pdf_text_diff <- diff_lines(normalize_pdf_text(outs[[1]]),
                                                normalize_pdf_text(outs[[2]]),
                                                paste0(prefix, ".pdf"))
                if (row$pdf_text_diff > 0) row$diff_file <- paste0(prefix, ".pdf.diff")
            } else {
                row$diff_file <- sprintf("output types differ: %s vs %s", basename(outs[[1]]), basename(outs[[2]]))
            }
        }
        rows[[length(rows) + 1L]] <- row
    }
}
res <- do.call(rbind, lapply(rows, as.data.frame))
if (is.null(res)) res <- data.frame(package = character(), vignette = character(),
                                    uses_biocstyle = logical(), baseline = character(),
                                    candidate = character(), html_diff = integer(),
                                    pdf_pages = character(), pdf_delta = integer(),
                                    pdf_text_diff = integer(), diff_file = character(),
                                    baseline_error = character(), candidate_error = character(),
                                    biocstyle_check = character())
write.table(res, file.path(run_dir, "results.tsv"), sep = "\t", quote = TRUE,
            row.names = FALSE, na = "")

## ---- Report ---------------------------------------------------------------------
rel <- function(p) ifelse(is.na(p), NA, sub(paste0("^", run_dir, "/"), "", p))
fmt <- function(x) ifelse(is.na(x), "", as.character(x))
first_line <- function(x) {
    x <- strsplit(x, "\n", fixed = TRUE)
    vapply(x, function(l) {
        l <- l[nzchar(trimws(l))]
        if (!length(l)) "" else gsub("|", "\\|", substr(l[[1]], 1, 160), fixed = TRUE)
    }, "")
}
ok <- res$baseline == "ok"
cok <- res$candidate == "ok"
new_fail <- res[ok & !cok, ]
fixed <- res[!ok & cok, ]
both_fail <- res[!ok & !cok, ]
changed <- res[ok & cok & ((!is.na(res$html_diff) & res$html_diff > 0) |
                           (!is.na(res$pdf_delta) & res$pdf_delta != 0) |
                           (!is.na(res$pdf_text_diff) & res$pdf_text_diff > 0) |
                           grepl("^output types differ", fmt(res$diff_file))), ]

timings <- read.delim(file.path(run_dir, "timings.tsv"))
setup_secs <- timings[timings$phase == "setup", ]
pkg_total <- timings[timings$phase == "package" & timings$step == "total", ]

md <- c(
    "# BiocStyle reverse-dependency vignette sweep", "",
    sprintf("- Baseline: %s", cfg$baseline),
    sprintf("- Candidate: %s", cfg$candidate),
    sprintf("- Started: %s; per-vignette timeout %s s; platform %s", cfg$started, cfg$timeout, cfg$platform),
    sprintf("- Packages: %d sampled (%s); %d vignettes compared",
            nrow(sample), if (nzchar(cfg$pkgs)) "requested via pkgs=" else
                sprintf("top %s by download score + forced.txt, from %s BiocStyle reverse deps",
                        cfg$n, sample$n_revdeps[[1]]),
            nrow(res)),
    sprintf("- Renders ok: baseline %d/%d, candidate %d/%d", sum(ok), nrow(res), sum(cok), nrow(res)),
    sprintf("- **New failures: %d · Fixed: %d · Changed output: %d** · Failing in both: %d",
            nrow(new_fail), nrow(fixed), nrow(changed), nrow(both_fail)),
    ""
)

section <- function(title, df, cols_fn, empty = "None.") {
    out <- c(sprintf("## %s (%d)", title, nrow(df)), "")
    if (!nrow(df)) return(c(out, empty, ""))
    c(out, cols_fn(df), "")
}

md <- c(md,
    section("NEW FAILURES (ok → error)", new_fail, function(d) c(
        "| package | vignette | candidate status | first error line |", "|---|---|---|---|",
        sprintf("| %s | %s | %s | %s |", d$package, d$vignette, d$candidate, first_line(d$candidate_error)))),
    section("FIXED (error → ok)", fixed, function(d) c(
        "| package | vignette | baseline status | first baseline error line |", "|---|---|---|---|",
        sprintf("| %s | %s | %s | %s |", d$package, d$vignette, d$baseline, first_line(d$baseline_error)))),
    section("Changed output (both ok)", changed, function(d) c(
        "| package | vignette | html diff lines | pdf pages | pdf text diff lines | diff |", "|---|---|---|---|---|---|",
        sprintf("| %s | %s | %s | %s | %s | %s |", d$package, d$vignette, fmt(d$html_diff),
                fmt(d$pdf_pages), fmt(d$pdf_text_diff), fmt(rel(d$diff_file))))),
    section("Failing in both (pre-existing or environment)", both_fail, function(d) c(
        "| package | vignette | baseline | candidate | first baseline error line |", "|---|---|---|---|---|",
        sprintf("| %s | %s | %s | %s | %s |", d$package, d$vignette, d$baseline, d$candidate,
                first_line(d$baseline_error))))
)

if (any(!is.na(res$biocstyle_check))) {
    bad <- res[!is.na(res$biocstyle_check), ]
    md <- c(md, "## WARNING: renders that loaded the wrong BiocStyle", "",
            sprintf("- %s / %s: %s", bad$package, bad$vignette, bad$biocstyle_check), "")
}
if (length(pkg_problems)) {
    md <- c(md, "## Package-level problems", "",
            sprintf("- %s: %s", names(pkg_problems), unlist(pkg_problems)), "")
}

md <- c(md, "## All vignettes", "",
    "| package | vignette | uses BiocStyle | baseline | candidate | html diff lines | pdf page delta | pdf pages | pdf text diff lines |",
    "|---|---|---|---|---|---|---|---|---|",
    sprintf("| %s | %s | %s | %s | %s | %s | %s | %s | %s |", res$package, res$vignette,
            ifelse(res$uses_biocstyle, "yes", "no"), res$baseline, res$candidate,
            fmt(res$html_diff), fmt(res$pdf_delta), fmt(res$pdf_pages), fmt(res$pdf_text_diff)),
    "")

md <- c(md, "## Timings", "",
    "| step | seconds |", "|---|---|",
    sprintf("| setup: %s | %.0f |", setup_secs$step, setup_secs$seconds),
    sprintf("| %s (download + install + render both) | %.0f |", pkg_total$package, pkg_total$seconds),
    "",
    "Per-step timings (download, install, render_baseline, render_candidate) are in `timings.tsv`;",
    "full results with error text are in `results.tsv`; normalized outputs and diffs are under `compare/`.",
    "")

writeLines(md, file.path(run_dir, "report.md"))
message("Wrote ", file.path(run_dir, "report.md"), ": ", nrow(new_fail), " new failures, ",
        nrow(fixed), " fixed, ", nrow(changed), " changed")
