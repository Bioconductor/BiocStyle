## use_vignette_quarto() and use_quarto_extension(): set up Quarto vignettes
## that use the BiocStyle Quarto extension and vignette engines.

#' Use the Bioconductor style in Quarto documents and vignettes
#'
#' BiocStyle ships a Quarto extension with the formats
#' \code{biocstyle-html} and \code{biocstyle-pdf}, and two vignette engines,
#' \code{BiocStyle::quarto_html} and \code{BiocStyle::quarto_pdf}, that render
#' \file{.qmd} vignettes with them.
#'
#' \code{use_vignette_quarto()} creates \file{vignettes/<name>.qmd} in a
#' package from a template with the vignette header for the chosen format, and
#' the structure of a package vignette (installation, support, session
#' information). It then calls \code{use_quarto_extension()}.
#'
#' \code{use_quarto_extension()} copies the extension into
#' \file{<dir>/_extensions/biocstyle}, so that \command{quarto render} and the
#' RStudio \sQuote{Render} button can find the formats when previewing a
#' vignette. Vignette engines do not need this copy: at build time they copy
#' the extension of the installed BiocStyle next to the vignette and remove it
#' afterwards. So that the copy is not part of the package sources, the
#' function adds \file{<dir>/_extensions} to the package's
#' \file{.Rbuildignore} and \file{.gitignore}, when those files exist, together
#' with \file{<dir>/Bioconductor.sty}, the LaTeX style file that rendering to
#' PDF copies next to the document. An
#' existing \file{_extensions} directory is never deleted; an existing
#' \file{_extensions/biocstyle} is only replaced when \code{overwrite = TRUE}.
#'
#' A package with Quarto vignettes using these engines needs, in its
#' \file{DESCRIPTION}:
#' \preformatted{
#'     VignetteBuilder: BiocStyle
#'     Suggests: BiocStyle, knitr, quarto
#' }
#' (with \code{VignetteBuilder: BiocStyle, knitr} if the package also has R
#' Markdown vignettes), and the Quarto command line tool
#' (\url{https://quarto.org}) installed where the vignettes are built.
#' Without the \pkg{quarto} R package or the Quarto tool, building the
#' vignette fails, except under \command{R CMD check}, where the engine warns
#' and writes a placeholder instead.
#'
#' See the vignette \dQuote{Authoring Quarto vignettes} for the features of
#' the formats and how they map to \code{BiocStyle::html_document} and
#' \code{BiocStyle::pdf_document}.
#'
#' @param name character(1) vignette name; the file is
#'     \file{vignettes/<name>.qmd}.
#' @param format character(1), \code{"html"} or \code{"pdf"}: the vignette
#'     format and engine (\code{BiocStyle::quarto_html} or
#'     \code{BiocStyle::quarto_pdf}).
#' @param title character(1) vignette title, also used as the vignette index
#'     entry.
#' @param package character(1) name of the package the vignette documents.
#'     Defaults to the \code{Package} field of \file{<path>/DESCRIPTION}.
#' @param path character(1) package directory.
#' @param dir character(1) directory to copy the extension into, usually the
#'     package's \file{vignettes} directory.
#' @param overwrite logical(1) replace an existing
#'     \file{<dir>/_extensions/biocstyle}.
#' @return \code{use_vignette_quarto()} returns the path of the new vignette,
#'     \code{use_quarto_extension()} the path of the extension copy; both
#'     invisibly.
#' @seealso \code{\link{use_vignette_html}} for R Markdown vignettes.
#' @examples
#' pkg <- file.path(tempdir(), "MyPackage")
#' dir.create(pkg)
#' writeLines(c("Package: MyPackage", "Version: 0.99.0"),
#'            file.path(pkg, "DESCRIPTION"))
#' vignette <- use_vignette_quarto("MyPackage", path = pkg)
#' cat(head(readLines(vignette), 20), sep = "\n")
#' list.files(file.path(pkg, "vignettes"), recursive = TRUE)
#' unlink(pkg, recursive = TRUE)
#' @name use_vignette_quarto
NULL

#' @rdname use_vignette_quarto
#' @export
use_vignette_quarto <- function(name, format = c("html", "pdf"),
                                title = paste("Introduction to", package),
                                package = NULL, path = ".") {
    stopifnot(
        is.character(name), length(name) == 1L, !is.na(name), nzchar(name),
        is.character(path), length(path) == 1L, dir.exists(path)
    )
    format <- match.arg(format)
    description <- file.path(path, "DESCRIPTION")
    bug_reports <- NA
    if (file.exists(description)) {
        dcf <- read.dcf(description, fields = c("Package", "BugReports"))
        if (is.null(package))
            package <- unname(dcf[1L, "Package"])
        bug_reports <- unname(dcf[1L, "BugReports"])
    }
    if (is.null(package) || is.na(package))
        package <- "MyPackage"
    stopifnot(is.character(title), length(title) == 1L, !is.na(title))
    if (is.na(bug_reports))
        bug_reports <- "https://support.bioconductor.org"

    vignettes <- file.path(path, "vignettes")
    destination <- file.path(vignettes, paste0(name, ".qmd"))
    if (file.exists(destination))
        stop("'", destination, "' already exists", call. = FALSE)
    dir.create(vignettes, showWarnings = FALSE, recursive = TRUE)

    template <- readUTF8(
        system.file("quarto", "templates", "vignette.qmd", package = "BiocStyle")
    )
    values <- c(
        title = title, package = package, format = format,
        bug_reports = bug_reports
    )
    for (key in names(values))
        template <- gsub(
            paste0("{{", key, "}}"), values[[key]], template, fixed = TRUE
        )
    writeUTF8(template, destination)

    use_quarto_extension(dir = vignettes)
    invisible(destination)
}

#' @rdname use_vignette_quarto
#' @export
use_quarto_extension <- function(dir = "vignettes", overwrite = FALSE) {
    stopifnot(
        is.character(dir), length(dir) == 1L, dir.exists(dir),
        is.logical(overwrite), length(overwrite) == 1L, !is.na(overwrite)
    )
    extensions <- file.path(dir, "_extensions")
    target <- file.path(extensions, "biocstyle")
    if (dir.exists(target)) {
        if (!overwrite) {
            message("'", target, "' already exists; use 'overwrite = TRUE' ",
                    "to replace it with BiocStyle ", packageVersion("BiocStyle"),
                    "'s copy")
        } else {
            unlink(target, recursive = TRUE)
            .copy_quarto_extension(extensions)
        }
    } else {
        .copy_quarto_extension(extensions)
    }

    ## keep the copy, and the Bioconductor.sty that rendering to PDF copies
    ## next to the document, out of the package sources
    root <- dirname(normalizePath(dir))
    relative <- basename(normalizePath(dir))
    .add_ignore_lines(
        file.path(root, ".Rbuildignore"),
        paste0("^", relative, c("/_extensions$", "/Bioconductor\\.sty$"))
    )
    .add_ignore_lines(
        file.path(root, ".gitignore"),
        paste0("/", relative, c("/_extensions/", "/Bioconductor.sty"))
    )
    invisible(target)
}

.add_ignore_lines <- function(file, lines_to_add) {
    if (!file.exists(file))
        return(invisible(FALSE))
    lines <- readUTF8(file)
    new <- setdiff(lines_to_add, lines)
    if (length(new))
        writeUTF8(c(lines, new), file)
    invisible(length(new) > 0L)
}
