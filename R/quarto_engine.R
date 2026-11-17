## Vignette engines BiocStyle::quarto_html and BiocStyle::quarto_pdf: render
## .qmd vignettes with the formats biocstyle-html and biocstyle-pdf of the
## Quarto extension in inst/quarto/_extensions/biocstyle.
##
## The engines are adapted from Bioconductor/BiocStyle#115 by Marcel Ramos
## (@LiNk-NY), itself adapted from quarto-dev/quarto-r's R/utils-vignettes.R.

.quarto_extension_dir <- function()
    system.file("quarto", "_extensions", "biocstyle", package = "BiocStyle")

## Register BiocStyle::quarto_html and BiocStyle::quarto_pdf; called from
## .onLoad().
.register_quarto_engines <- function() {
    ## tangle and spell-check like knitr's R Markdown engine
    rmd_engine <- tools::vignetteEngine("rmarkdown", package = "knitr")
    for (format in c("html", "pdf"))
        tools::vignetteEngine(
            paste0("quarto_", format),
            weave = .quarto_weave(format),
            tangle = rmd_engine$tangle,
            pattern = "[.]qmd$",
            package = "BiocStyle",
            aspell = rmd_engine$aspell
        )
}

.is_R_CMD_check <- function() {
    !is.na(Sys.getenv("_R_CHECK_PACKAGE_NAME_", NA)) ||
        tolower(Sys.getenv("_R_CHECK_LICENSE_")) == "true"
}

.quarto_available <- function() {
    requireNamespace("quarto", quietly = TRUE) &&
        !is.null(quarto::quarto_path())
}

## Under R CMD check without Quarto, write a placeholder so the check can
## continue (the same approach as quarto's own vignette engine).
.quarto_placeholder <- function(file) {
    out <- paste0(tools::file_path_sans_ext(basename(file)), ".html")
    writeLines(
        "The vignette could not be built because Quarto is not available.",
        out
    )
    out
}

## Version of the package named in the 'package:' field of the document:
## from ../DESCRIPTION when it describes that package (the package being
## built), otherwise from the installed package.
.quarto_package_version <- function(input) {
    package <- rmarkdown::yaml_front_matter(input)$package
    if (!is.character(package) || length(package) != 1L)
        return(NULL)
    package <- trimws(package)
    if (!grepl("^[[:alnum:].]+[[:alnum:]]+$", package))
        return(NULL)
    description <- file.path(dirname(input), "..", "DESCRIPTION")
    if (file.exists(description)) {
        dcf <- read.dcf(description, fields = c("Package", "Version"))
        if (identical(unname(dcf[1L, "Package"]), package) &&
            !is.na(dcf[1L, "Version"]))
            return(unname(dcf[1L, "Version"]))
    }
    tryCatch(
        as.character(packageVersion(package)),
        error = function(e) NULL
    )
}

## Make the biocstyle extension available next to 'input'. A user's existing
## _extensions/biocstyle is used as is. Returns the paths this call created,
## for removal after rendering.
.quarto_stage_extension <- function(input) {
    extensions <- file.path(dirname(input), "_extensions")
    target <- file.path(extensions, "biocstyle")
    if (dir.exists(target))
        return(character())
    created <- if (dir.exists(extensions)) target else extensions
    .copy_quarto_extension(extensions)
    created
}

.copy_quarto_extension <- function(extensions) {
    dir.create(extensions, showWarnings = FALSE, recursive = TRUE)
    ok <- file.copy(.quarto_extension_dir(), extensions, recursive = TRUE)
    if (!ok)
        stop("could not copy the BiocStyle Quarto extension to '",
             extensions, "'", call. = FALSE)
    file.path(extensions, "biocstyle")
}

.quarto_weave <- function(format) {
    force(format)
    function(file, driver, syntax, encoding, quiet = FALSE, ...) {
        if (!.quarto_available()) {
            msg <- paste(
                "Building Quarto vignettes needs the 'quarto' R package and",
                "the Quarto command line tool (https://quarto.org)."
            )
            if (!.is_R_CMD_check())
                stop(msg, call. = FALSE)
            warning(msg, call. = FALSE)
            return(.quarto_placeholder(file))
        }

        input <- normalizePath(file, mustWork = TRUE)
        input_dir <- dirname(input)

        ## files this render may create next to the input that are not
        ## outputs: the staged extension and the LaTeX style file that the
        ## PDF format copies alongside the document
        sty <- file.path(input_dir, "Bioconductor.sty")
        created <- .quarto_stage_extension(input)
        if (!file.exists(sty))
            created <- c(created, sty)
        on.exit(unlink(created, recursive = TRUE), add = TRUE)

        ## the R process started by Quarto should see this session's
        ## libraries, e.g. a temporary library used by R CMD build/check
        r_libs <- Sys.getenv("R_LIBS", NA)
        Sys.setenv(R_LIBS = paste(.libPaths(), collapse = .Platform$path.sep))
        on.exit(
            if (is.na(r_libs)) Sys.unsetenv("R_LIBS")
            else Sys.setenv(R_LIBS = r_libs),
            add = TRUE
        )

        metadata <- list()
        version <- .quarto_package_version(input)
        if (!is.null(version))
            metadata[["package-version"]] <- version

        quarto::quarto_render(
            input,
            output_format = paste0("biocstyle-", format),
            metadata = if (length(metadata)) metadata,
            quiet = quiet,
            as_job = FALSE
        )

        output <- paste0(
            tools::file_path_sans_ext(basename(input)), ".", format
        )
        ## tools::buildVignette() expects the output in the working directory
        if (!identical(normalizePath(getwd()), input_dir))
            file.rename(file.path(input_dir, output), output)
        invisible(output)
    }
}

