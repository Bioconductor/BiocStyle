## BiocStyle vignette engine registration
## Adapted from github.com/quarto-dev/quarto-r/blob/main/R/utils-vignettes.R
register_vignette_engines <- function(pkg) {
    vig_engine("html", quarto_format = "html")
    vig_engine("pdf", quarto_format = "pdf")
}

vig_engine <- function(..., quarto_format) {
    rmd_eng <- tools::vignetteEngine("rmarkdown", package = "knitr")
    tools::vignetteEngine(
        ...,
        weave = vweave_quarto(quarto_format),
        tangle = rmd_eng$tangle,
        pattern = "[.]qmd$",
        package = "BiocStyle",
        aspell = rmd_eng$aspell
    )
}

vweave_quarto <- function(format) {
    meta <- get_meta(format)
    function(file, driver, syntax, encoding, quiet = FALSE, ...) {
        # protect if Quarto is not installed
        if (is.null(quarto::quarto_path())) {
            msg <- c(
                paste(
                    "Quarto binary is required to build Quarto vignettes",
                    "but is not available.",
                ),
                i = paste(
                    "Please make sure it is installed and found",
                    "by {.code find_quarto()}."
                )
            )
            if (is_R_CMD_check()) {
                cli::cli_inform(msg)
            } else {
                cli::cli_abort(msg, call = NULL)
            }
            return(vweave_empty(file))
        }

        # Log debug information using the new configurable logging function
        quarto:::quarto_log("R_LIBS: ", Sys.getenv("R_LIBS"))
        quarto:::quarto_log(
            ".libPaths(): ",
            paste0(.libPaths(), collapse = ":")
        )
        quarto:::quarto_log(
            "Packages: ",
            paste0(dir(.libPaths()[1]), collapse = ",")
        )

        quarto::quarto_render(
            file,
            ...,
            output_format = format,
            metadata = meta
        )
    }
}

get_meta <- function(format) {
    if (is.null(format)) {
        return(NULL)
    }
    if (format == "html") {
        return(get_meta_for_html())
    }
    if (format == "pdf") {
        return(get_meta_for_pdf())
    }
}

get_meta_for_pdf <- function() {
    temp_dir <- tempfile(pattern = "quarto-biocstyle")
    dir.create(temp_dir)

    resources_path <- system.file("resources", "tex", package = "BiocStyle")
    highlighting_macros_path <-
        file.path(resources_path, "highlighting-macros.def")

    template_path <- system.file(
        "resources", "tex", "template.tex",
        package = "BiocStyle", mustWork = TRUE
    )
    if (!file.exists(template_path))
        cli::cli_abort("Missing custom BiocStyle LaTeX template.")

    bst <-
        system.file("resources", "tex", "unsrturl.bst", package = "BiocStyle")
    # 3. Create a temporary header file for the highlighting macros
    # This is like the `head` creation in BiocStyle's R Markdown function.
    header_file <- file.path(temp_dir, "header.tex")
    highlight_content <- readLines(highlighting_macros_path)
    head <- c(
        "% code highlighting for BiocStyle",
        highlight_content
    )
    head <- c(
        head,
        sprintf(
            "\\AtBeginDocument{\\bibliographystyle{%s}}\n",
            sub(".bst$", "", bst)
        )
    )
    writeLines(head, con = header_file)

    # 4. Copy the .sty and .bst files to the temporary directory so LaTeX can find them
    file.copy(
        system.file(
            "resources", "tex", "Bioconductor.sty", package = "BiocStyle"
        ),
        temp_dir
    )
    file.copy(bst, temp_dir)
    meta <- list()
    meta$format$pdf <- list(
        toc = TRUE,
        `number-sections` = TRUE,
        template = template_path,
        # don't try to install CTAN package on CRAN environment
        `latex-auto-install` = !is_cran_check(),
        `keep-tex` = TRUE,
        # `biblio-style` = "unstrurl",
        `include-in-header` = header_file
        # linkcolor = "BiocStyle-Link",
        # citecolor = "BiocStyle-Cite",
        # urlcolor = "BiocStyle-URL"
    )
    meta
}

get_meta_for_html <- function() {
    css <- system.file(
        "resources",
        "html",
        "biocstyle.css",
        package = "BiocStyle"
    )
    scss <- system.file(
        "resources",
        "html",
        "biocstyle.scss",
        package = "BiocStyle"
    )
    meta <- list()
    meta$format$html <-
        list(
            `embed-resources` = TRUE,
            minimal = TRUE,
            toc = TRUE,
            `toc-float` = TRUE,
            `toc-depth` = 3L,
            `toc-location` = "left",
            `number-sections` = TRUE,
            theme = scss,
            css = css
        )
    meta
}

is_R_CMD_check <- function() {
    !is.na(Sys.getenv("_R_CHECK_PACKAGE_NAME_", NA)) ||
        tolower(Sys.getenv("_R_CHECK_LICENSE_")) == "true"
}

# from knitr internal
is_cran_check <- function() {
    is_cran() && is_R_CMD_check()
}

is_cran <- function() {
    !rlang::is_interactive() &&
        !isTRUE(as.logical(Sys.getenv("NOT_CRAN", "false")))
}

# trick from knitr to avoid problem on R CMD check (e.g. when no Quarto
# available) It will silently skip the vignette
vweave_empty <- function(file, ..., .reason = "Quarto") {
    out <- sprintf("%s.html", tools::file_path_sans_ext(basename(file)))
    writeLines(
        sprintf(
            "The vignette could not be built because %s is not available.",
            .reason
        ),
        out
    )
    out
}
