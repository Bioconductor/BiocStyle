latex <-
    function(...)
{
    fl <- system.file(package="BiocStyle", "sty", "Bioconductor.sty")
    cat(sprintf("\\usepackage{%s}", sub(".sty$", "", fl)))
}
