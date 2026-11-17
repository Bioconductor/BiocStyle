## Tests for the Quarto extension, the BiocStyle::quarto_html and
## BiocStyle::quarto_pdf vignette engines, and use_vignette_quarto() /
## use_quarto_extension(). Rendering tests are deactivated when the quarto
## R package or the Quarto command line tool is not available.

.quarto_skip_reason <- function() {
    if (!requireNamespace("quarto", quietly = TRUE))
        return("the 'quarto' R package is not installed")
    if (is.null(quarto::quarto_path()))
        return("the Quarto command line tool is not available")
    NULL
}

.quarto_rendered <- new.env()

## Build inst/unitTests/cases/quarto_parity.qmd with the vignette engine for
## 'format' (once per session) and return the output path.
.quarto_render_case <- function(format = c("html", "pdf")) {
    format <- match.arg(format)
    if (!is.null(.quarto_rendered[[format]]))
        return(.quarto_rendered[[format]])
    src <- tempfile("quarto-src-")
    out <- tempfile("quarto-out-")
    dir.create(src)
    dir.create(out)
    case <- system.file("unitTests", "cases", "quarto_parity.qmd",
                        package = "BiocStyle")
    file.copy(case, src)
    engine <- tools::vignetteEngine(paste0("quarto_", format),
                                    package = "BiocStyle")
    tools::buildVignette(file.path(src, basename(case)), dir = out,
                         engine = engine, tangle = FALSE)
    output <- file.path(out, paste0("quarto_parity.", format))
    ## the engine leaves nothing behind next to the input
    checkIdentical("quarto_parity.qmd", list.files(src, all.files = TRUE,
                                                   no.. = TRUE))
    .quarto_rendered[[format]] <- output
    output
}

.quarto_html <- function() {
    paste(readLines(.quarto_render_case("html"), warn = FALSE),
          collapse = "\n")
}

.has <- function(pattern, text, ...) grepl(pattern, text, ...)

## Percent-decode a string; utils::URLdecode() is too slow for the
## embedded stylesheets
.url_decode <- function(x) {
    bytes <- charToRaw(x)
    pct <- which(bytes == charToRaw("%"))
    pct <- pct[pct + 2L <= length(bytes)]
    hex <- strtoi(paste0(rawToChar(bytes[pct + 1L], multiple = TRUE),
                         rawToChar(bytes[pct + 2L], multiple = TRUE)), 16L)
    pct <- pct[!is.na(hex)]
    hex <- hex[!is.na(hex)]
    if (!length(pct))
        return(x)
    bytes[pct] <- as.raw(hex)
    rawToChar(bytes[-c(pct + 1L, pct + 2L)])
}

test_quarto_extension_sty_matches_resources <- function() {
    ## the extension carries its own copy of Bioconductor.sty
    sty <- system.file("resources", "tex", "Bioconductor.sty",
                       package = "BiocStyle")
    ext_sty <- system.file("quarto", "_extensions", "biocstyle",
                           "Bioconductor.sty", package = "BiocStyle")
    checkIdentical(readLines(sty), readLines(ext_sty))
}

test_quarto_engines_registered <- function() {
    for (name in c("quarto_html", "quarto_pdf")) {
        engine <- tools::vignetteEngine(name, package = "BiocStyle")
        checkIdentical("[.]qmd$", engine$pattern)
        checkTrue(is.function(engine$weave))
        checkTrue(is.function(engine$tangle))
    }
}

test_quarto_html_parity <- function() {
    reason <- .quarto_skip_reason()
    if (!is.null(reason))
        DEACTIVATED(reason)
    html <- .quarto_html()
    version <- as.character(packageVersion("BiocStyle"))

    ## title and subtitle
    checkTrue(.has('<h1 class="title">Parity reference vignette</h1>', html,
                   fixed = TRUE))
    checkTrue(.has('class="subtitle[^"]*">Same content, two engines', html))
    ## authors, affiliations and email
    checkTrue(.has('<p class="author-name">', html, fixed = TRUE))
    checkTrue(.has("Ada Lovelace", html, fixed = TRUE))
    checkTrue(.has("Charles Babbage", html, fixed = TRUE))
    checkTrue(.has('class="author-affiliation"', html, fixed = TRUE))
    checkTrue(.has("Analytical Engine Institute, London, UK", html,
                   fixed = TRUE))
    checkTrue(.has("Difference Engine Lab, Cambridge, UK", html, fixed = TRUE))
    checkTrue(.has('href="mailto:ada@example.org"', html, fixed = TRUE))
    ## abstract
    checkTrue(.has('<div class="abstract">', html, fixed = TRUE))
    checkTrue(.has("A short abstract describing", html, fixed = TRUE))
    ## package line with the version passed by the engine
    checkTrue(.has(sprintf("<p>BiocStyle %s</p>", version), html,
                   fixed = TRUE))
    ## table of contents on the left
    checkTrue(.has('id="quarto-sidebar-toc-left"', html, fixed = TRUE))
    checkTrue(.has('id="TOC"', html, fixed = TRUE))
    ## numbered sections
    checkTrue(.has('<span class="header-section-number">1</span>', html,
                   fixed = TRUE))
    checkTrue(.has('<span class="header-section-number">1.1</span>', html,
                   fixed = TRUE))
    ## footnote in the margin
    checkTrue(.has('column-margin[^"]*"><div id="fn1">', html))
    ## figure size classes
    checkTrue(.has('<div class="cell fig-small', html, fixed = TRUE))
    checkTrue(.has('<div class="cell fig-wide[^"]*page-columns', html))
    ## wide table wrapped in a scrolling container
    checkTrue(.has('<div class="horizontal-scroll">\\s*<table', html))
    ## Biocpkg() and friends via inline R
    checkTrue(.has(
        'href="https://bioconductor.org/packages/[^"]*SummarizedExperiment"',
        html))
    checkTrue(.has("https://CRAN.R-project.org/package=knitr", html,
                   fixed = TRUE))
    ## CSS compiled from the extension's SCSS is embedded (as URL-encoded
    ## data URIs)
    css <- regmatches(html, gregexpr('data:text/css,[^"]+', html))[[1L]]
    css <- paste(vapply(css, .url_decode, character(1L)), collapse = "\n")
    checkTrue(.has("#1a81c2", css, ignore.case = TRUE))
    checkTrue(.has("#87b13f", css, ignore.case = TRUE))
    checkTrue(.has(".horizontal-scroll", css, fixed = TRUE))
}

test_quarto_pdf_parity <- function() {
    reason <- .quarto_skip_reason()
    if (!is.null(reason))
        DEACTIVATED(reason)
    pdf <- .quarto_render_case("pdf")
    checkTrue(file.exists(pdf))
    checkIdentical("%PDF", readChar(pdf, 4L, useBytes = TRUE))

    pdftotext <- Sys.which("pdftotext")
    if (!nzchar(pdftotext))
        return(invisible())
    txt <- system2(pdftotext, c(shQuote(pdf), "-"), stdout = TRUE)
    txt <- paste(txt, collapse = "\n")
    version <- as.character(packageVersion("BiocStyle"))
    checkTrue(.has("Parity reference vignette", txt, fixed = TRUE))
    checkTrue(.has("Same content, two engines", txt, fixed = TRUE))
    checkTrue(.has("Ada Lovelace", txt, fixed = TRUE))
    checkTrue(.has("Analytical Engine Institute", txt, fixed = TRUE))
    checkTrue(.has("Abstract", txt, fixed = TRUE))
    checkTrue(.has("Package", txt, fixed = TRUE))
    checkTrue(.has(paste("BiocStyle", version), txt, fixed = TRUE))
    checkTrue(.has("Contents", txt, fixed = TRUE))
}

## A minimal package directory for the use_*() helpers
.quarto_temp_package <- function(name = "MyPkg", version = "0.99.3") {
    path <- file.path(tempfile("quarto-pkg-"), name)
    dir.create(path, recursive = TRUE)
    writeLines(c(paste("Package:", name), paste("Version:", version),
                 "BugReports: https://github.com/me/MyPkg/issues"),
               file.path(path, "DESCRIPTION"))
    writeLines("^.*\\.Rproj$", file.path(path, ".Rbuildignore"))
    writeLines(".Rproj.user", file.path(path, ".gitignore"))
    path
}

test_use_vignette_quarto <- function() {
    pkg <- .quarto_temp_package()
    on.exit(unlink(dirname(pkg), recursive = TRUE))

    vignette <- BiocStyle::use_vignette_quarto("intro", path = pkg)
    checkTrue(file.exists(vignette))
    checkIdentical(file.path(pkg, "vignettes", "intro.qmd"), vignette)
    lines <- readLines(vignette)
    checkTrue(any(lines == 'title: "Introduction to MyPkg"'))
    checkTrue(any(lines == "package: MyPkg"))
    checkTrue(any(lines == "format: biocstyle-html"))
    checkTrue(any(grepl("%\\VignetteIndexEntry{Introduction to MyPkg}",
                        lines, fixed = TRUE)))
    checkTrue(any(grepl("%\\VignetteEngine{BiocStyle::quarto_html}", lines,
                        fixed = TRUE)))
    checkTrue(any(grepl("BiocManager::install(\"MyPkg\")", lines,
                        fixed = TRUE)))
    checkTrue(any(grepl("https://github.com/me/MyPkg/issues", lines,
                        fixed = TRUE)))
    checkTrue(!any(grepl("{{", lines, fixed = TRUE)))

    ## the extension is copied for interactive rendering, and kept out of
    ## the package sources
    checkTrue(file.exists(file.path(pkg, "vignettes", "_extensions",
                                    "biocstyle", "_extension.yml")))
    checkTrue("^vignettes/_extensions$" %in%
              readLines(file.path(pkg, ".Rbuildignore")))
    checkTrue("/vignettes/_extensions/" %in%
              readLines(file.path(pkg, ".gitignore")))

    ## PDF variant; existing vignettes are not overwritten
    pdf <- suppressMessages(BiocStyle::use_vignette_quarto(
        "intro-pdf", format = "pdf", title = "PDF vignette", path = pkg
    ))
    lines <- readLines(pdf)
    checkTrue(any(lines == "format: biocstyle-pdf"))
    checkTrue(any(grepl("%\\VignetteEngine{BiocStyle::quarto_pdf}", lines,
                        fixed = TRUE)))
    checkTrue(any(grepl("%\\VignetteIndexEntry{PDF vignette}", lines,
                        fixed = TRUE)))
    checkException(BiocStyle::use_vignette_quarto("intro", path = pkg),
                   silent = TRUE)
    ## ignore entries are added only once
    checkIdentical(1L, sum(readLines(file.path(pkg, ".Rbuildignore")) ==
                           "^vignettes/_extensions$"))
}

test_use_quarto_extension_keeps_existing <- function() {
    pkg <- .quarto_temp_package()
    on.exit(unlink(dirname(pkg), recursive = TRUE))
    vignettes <- file.path(pkg, "vignettes")
    other <- file.path(vignettes, "_extensions", "other")
    dir.create(other, recursive = TRUE)
    writeLines("keep me", file.path(other, "_extension.yml"))

    BiocStyle::use_quarto_extension(vignettes)
    marker <- file.path(vignettes, "_extensions", "biocstyle", "marker")
    writeLines("local edit", marker)
    checkTrue(file.exists(file.path(other, "_extension.yml")))

    ## without overwrite, an existing copy is left alone
    suppressMessages(BiocStyle::use_quarto_extension(vignettes))
    checkTrue(file.exists(marker))

    ## overwrite replaces only _extensions/biocstyle
    BiocStyle::use_quarto_extension(vignettes, overwrite = TRUE)
    checkTrue(!file.exists(marker))
    checkTrue(file.exists(file.path(vignettes, "_extensions", "biocstyle",
                                    "_extension.yml")))
    checkIdentical("keep me", readLines(file.path(other, "_extension.yml")))
}

test_quarto_engine_keeps_existing_extensions <- function() {
    reason <- .quarto_skip_reason()
    if (!is.null(reason))
        DEACTIVATED(reason)
    case <- system.file("unitTests", "cases", "quarto_parity.qmd",
                        package = "BiocStyle")
    engine <- tools::vignetteEngine("quarto_html", package = "BiocStyle")

    ## an _extensions directory without biocstyle: the engine adds
    ## biocstyle for the render and removes only that
    src <- tempfile("quarto-src-")
    other <- file.path(src, "_extensions", "other")
    dir.create(other, recursive = TRUE)
    other_yml <- c("title: Other", "version: 1.0.0", "contributes:",
                   "  shortcodes:", "    - other.lua")
    writeLines(other_yml, file.path(other, "_extension.yml"))
    writeLines("return {}", file.path(other, "other.lua"))
    file.copy(case, src)
    out <- tools::buildVignette(file.path(src, "quarto_parity.qmd"),
                                dir = src, engine = engine, tangle = FALSE)
    checkTrue(file.exists(file.path(src, "quarto_parity.html")))
    checkIdentical(other_yml, readLines(file.path(other, "_extension.yml")))
    checkTrue(!dir.exists(file.path(src, "_extensions", "biocstyle")))
    unlink(src, recursive = TRUE)

    ## a user's _extensions/biocstyle is used as is and not removed
    src <- tempfile("quarto-src-")
    dir.create(file.path(src, "_extensions"), recursive = TRUE)
    file.copy(system.file("quarto", "_extensions", "biocstyle",
                          package = "BiocStyle"),
              file.path(src, "_extensions"), recursive = TRUE)
    marker <- file.path(src, "_extensions", "biocstyle", "marker")
    writeLines("local copy", marker)
    file.copy(case, src)
    tools::buildVignette(file.path(src, "quarto_parity.qmd"), dir = src,
                         engine = engine, tangle = FALSE)
    checkTrue(file.exists(file.path(src, "quarto_parity.html")))
    checkTrue(file.exists(marker))
    unlink(src, recursive = TRUE)
}

test_quarto_package_version_from_description <- function() {
    ## the version comes from ../DESCRIPTION when it describes the package
    ## named in 'package:', otherwise from the installed package
    pkg <- .quarto_temp_package("BiocStyle", "9.9.9")
    on.exit(unlink(dirname(pkg), recursive = TRUE))
    dir.create(file.path(pkg, "vignettes"))
    qmd <- file.path(pkg, "vignettes", "v.qmd")
    writeLines(c("---", "title: x", "package: BiocStyle", "---"), qmd)
    checkIdentical("9.9.9", BiocStyle:::.quarto_package_version(qmd))

    writeLines(c("Package: Other", "Version: 1.0.0"),
               file.path(pkg, "DESCRIPTION"))
    checkIdentical(as.character(packageVersion("BiocStyle")),
                   BiocStyle:::.quarto_package_version(qmd))

    writeLines(c("---", "title: x", "---"), qmd)
    checkTrue(is.null(BiocStyle:::.quarto_package_version(qmd)))
}

test_quarto_preview_reads_description <- function() {
    ## rendering outside the engine (quarto render, RStudio Render button)
    ## takes the version from ../DESCRIPTION
    reason <- .quarto_skip_reason()
    if (!is.null(reason))
        DEACTIVATED(reason)
    pkg <- .quarto_temp_package("MyPkg", "0.99.3")
    on.exit(unlink(dirname(pkg), recursive = TRUE))
    vignettes <- file.path(pkg, "vignettes")
    dir.create(vignettes)
    BiocStyle::use_quarto_extension(vignettes)
    qmd <- file.path(vignettes, "preview.qmd")
    writeLines(c("---", "title: Preview", "package: MyPkg",
                 "format: biocstyle-html", "---", "", "# Section", "",
                 "Text."), qmd)
    quarto::quarto_render(qmd, quiet = TRUE, as_job = FALSE)
    html <- paste(readLines(file.path(vignettes, "preview.html"), warn = FALSE),
                  collapse = "\n")
    checkTrue(grepl("<p>MyPkg 0.99.3</p>", html, fixed = TRUE))
}

test_quarto_placeholder <- function() {
    ## used under R CMD check when Quarto is not available
    owd <- setwd(tempdir())
    on.exit(setwd(owd))
    out <- BiocStyle:::.quarto_placeholder("some/dir/vig.qmd")
    checkIdentical("vig.html", out)
    checkTrue(grepl("Quarto is not available", readLines(out)))
    unlink(out)
}
