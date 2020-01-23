#' @importFrom rmarkdown draft
.vignette_template <-
    function(destination, mode = c("html", "pdf"))
{
    stopifnot(
        is.character(destination), length(destination) == 1L, !is.na(destination),
        !file.exists(destination), file.exists(dirname(destination))
    )
    mode <- match.arg(mode)

    doc <- paste(mode, "document", sep = "_")
    rmarkdown::draft(
        destination, template = doc, package = "BiocStyle", edit = FALSE
    )
    destination
}

#' @title Create 'Rmd' vignette templates for HTML or PDF output
#'
#' @rdname use_vignette
#' 
#' @param destination character(1) file path to destination. The 
#'     directory of the destination must already exist, and the file 
#'     must not. The default creates a file in the temporary 
#'     directory, and so is removed when the R session ends.
#' 
#' @examples
#' use_vignette_html()
#'
#' @export
use_vignette_html <- 
    function(destination = tempfile(fileext = ".Rmd")) 
{
    .vignette_template(destination, "html")
}

#' @rdname use_vignette
#'
#' @export
use_vignette_pdf <- 
    function(destination = tempfile(fileext = ".Rmd")) 
{
    .vignette_template(destination, "pdf")
}
