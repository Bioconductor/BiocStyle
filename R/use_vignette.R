#' Create vignettes that utilize the BiocStyle templates
#'
#' Functions to create a skeleton Rmd or pdf vignette file.
#' @rdname use_vignette
#' @name use_vignette
#' @param vignette_name character, the name of the vignette
#' @return logical, indicating the vignette file was created successfully
#' @examples
#' use_vignette_rmd("testpkg_vignette")
#' @export
use_vignette_rmd <- function(vignette_name) {
    stopifnot(is.character(vignette_name))

    rmd = system.file(package = "BiocStyle", "rmarkdown", "templates", 
        "html_document", "skeleton", "skeleton.Rmd")

    file.copy(rmd, paste0(vignette_name, ".Rmd"))
}

#' @rdname use_vignette
#' @name use_vignette 
#' @examples
#'
#' use_vignette_pdf("testpkg_vignette")
#' @export
use_vignette_pdf <- function(vignette_name) {
    stopifnot(is.character(vignette_name))

    rmd = system.file(package = "BiocStyle", "rmarkdown", "templates", 
    "pdf_document", "skeleton", "skeleton.Rmd")

    file.copy(rmd, paste0(vignette_name, ".pdf"))
}
