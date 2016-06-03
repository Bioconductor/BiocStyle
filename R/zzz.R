resources <- bioconductor.sty <- bioconductor2.sty <- bioconductor.css <- bioconductor2.css <- NULL

rmarkdown_html_document <- rmarkdown::html_document

# resolve paths once during package load
.onLoad <- function(...) {
    resources <<- system.file(package = "BiocStyle", "resources")
    bioconductor.sty <<- file.path(resources, "tex", "Bioconductor.sty")
    bioconductor2.sty <<- file.path(resources, "tex", "Bioconductor2.sty")
    bioconductor.css <<- file.path(resources, "html", "bioconductor.css")
    bioconductor2.css <<- file.path(resources, "html", "bioconductor2.css")
    
    ## modify the original rmarkdown html_document format function
    hd_body <- body(rmarkdown::html_document)
    output_format <- grep("output_format", hd_body)
    body(rmarkdown_html_document)[[output_format]]$base_format$template <<- "default"
}
