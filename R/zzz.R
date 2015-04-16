.bioconductor.sty <- .bioconductor.css <- NULL

.onLoad <-
    function(...)
{
    .bioconductor.sty <<- system.file(package = "BiocStyle",
                                      "resources", "latex", "Bioconductor.sty")
    .bioconductor.css <<- system.file(package = "BiocStyle",
                                      "resources", "html", "bioconductor.css")
}

