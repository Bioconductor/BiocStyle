resources <- bioconductor.sty <- bioconductor.css <- NULL

# resolve paths once during package load
.onLoad <- function(lib, pkg) {
    register_vignette_engines(pkg)
    resources <<- system.file(package = "BiocStyle", "resources")
    bioconductor.sty <<- file.path(resources, "tex", "Bioconductor.sty")
    bioconductor.css <<- file.path(resources, "html", "bioconductor.css")
}
