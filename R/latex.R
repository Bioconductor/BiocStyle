latex <-
    function(..., width=90)
{
    options(..., width=width)
    fl <- system.file(package="BiocStyle", "sty", "Bioconductor.sty")
    cat(sprintf("\\usepackage{%s}", sub(".sty$", "", fl)))
}
