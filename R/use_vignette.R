#' @rdname use_vignette
#'
#' @title Create 'Rmd' vignette templates for HTML or PDF output
#'
#' @name use_vignette
#'
#' @param destination character(1) file path to destination, usually
#'     inside the `"vignettes"` directory of `package`. The directory
#'     of the destination must already exist, and the file must
#'     not. The default creates a file in the temporary directory, and
#'     so is removed when the R session ends.
#'
#' @param package character(1) package name.
#'
#' @param title character(1) vignette title.
#'
#' @param abstract character(1) paragraph-length summary of the vignette.
#'
#' @param authors person() vignette authors. Use `person(comment =
#'     ...)` to include author affiliation (see examples).
#'
#' @param bug_report_url character(1) url for bug reports (e.g.,
#'     `"https://github.com/Bioconductor/BiocStyle/issues"`)
#'
#' @examples
#' vignette <- use_package_vignette_html(
#'     authors = c(
#'         person(
#'             "Iman", "Author",
#'             email = "iman.author@some.where",
#'             comment = c(affiliation = "Prestigious Institution")
#'         ),
#'         person("Iman", "Other")
#'     )
#' )
#' vignette
#' cat(readLines(vignette), "\n")
NULL

#' @importFrom rmarkdown draft
.vignette_render <-
    function(destination, output = c("html", "pdf"))
{
     stopifnot(
         is.character(destination), length(destination) == 1L,
         !is.na(destination),
         !file.exists(destination), file.exists(dirname(destination))
     )
     output <- match.arg(output)

     doc <- paste(output, "document", sep = "_")
     draft(destination, template = doc, package = "BiocStyle", edit = FALSE)
     destination
}

#' @rdname use_vignette
#'
#' @description `use_vignette_html()` and `use_vignette_pdf()` create
#'     markdown vignettte templates for general use, in html or pdf
#'     formats. The template outlines markdown syntax and unique
#'     features enabled by BiocStyle.
#'
#' @export
use_vignette_html <-
    function(destination = tempfile(fileext = ".Rmd"))
{
    .vignette_render(destination, "html")
}

#' @rdname use_vignette
#'
#' @export
use_vignette_pdf <-
    function(destination = tempfile(fileext = ".Rmd"))
{
    .vignette_render(destination, "pdf")
}

#' @importFrom utils person
#'
#' @importFrom whisker whisker.render
.package_vignette_render <-
    function(
        destination, package, title, abstract, authors, bug_report_url,
        output = c("html", "pdf")
    )
{
    stopifnot(
        length(destination) == 1L,
        dir.exists(dirname(destination)), !file.exists(destination),
        is.character(package), length(package) == 1L,
        is.character(title), length(title) == 1L,
        is.character(abstract), length(abstract) == 1L,
        inherits(authors, "person"),
        is.character(bug_report_url), length(bug_report_url) == 1L
    )
    output <- match.arg(output)

    ## processing to list of authors, each author markdown formatted
    authors_data <- format(
        authors,
        fields = c("given", "family", "email", "comment"),
        braces = list(
            given = c("- name: ", ""),
            email = c("\n  email: ", ""),
            comment = c("\n  affilation: ", "")
        )
    )
    authors <- lapply(authors_data, function(author) {
        author <- gsub("[[:space:]]+\n", "\n", author) # trailing whitespace
        list(author = author)
    })

    ## data for whisker variable substitution
    data <- list(
        package = package,
        title = title,
        abstract = abstract,
        authors = authors,
        bug_report_url = bug_report_url,
        output = output
    )

    ## draft via rmarkdown
    whisker <- system.file(
        package = "BiocStyle", "rmarkdown", "templates", "package_vignette",
        "skeleton.Rmd.whisker"
    )

    ## inject values via whisker
    rendered <- whisker.render(readLines(whisker), data)
    writeLines(rendered, destination)

    invisible(destination)
}

#' @rdname use_vignette
#'
#' @description `use_package_vignette_html()` and
#'     `use_package_vignette_pdf()` create vignette templates suitable
#'     for inclusion in Bioconductor packages. The templates contain
#'     package installation instructions, suggest overall structure,
#'     and provide locations to seek support and create bug reports.
#'
#' @export
use_package_vignette_html <-
    function(
        destination = tempfile(fileext = ".Rmd"),
        package = "MyPackage",
        title = paste("Introduction to", package),
        abstract = "Describe your vignette with a paragraph-length summary.",
        authors = person(),
        bug_report_url = "https://support.bioconductor.org"
    )
{
    .package_vignette_render(
        destination, package, title, abstract, authors, bug_report_url, "html"
    )
}


#' @rdname use_vignette
#'
#' @export
use_package_vignette_pdf <-
    function(
        destination = tempfile(fileext = ".Rmd"),
        package = "MyPackage",
        title = paste("Introduction to", package),
        abstract = "Describe your vignette with a paragraph-length summary.",
        authors = person(),
        bug_report_url = "https://support.bioconductor.org"
    )
{
    .package_vignette_render(
        destination, package, title, abstract, authors, bug_report_url, "pdf"
    )
}
