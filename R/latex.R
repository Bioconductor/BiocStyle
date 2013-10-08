latex <-
    function(..., width=90, short.fignames=FALSE, fig.path)
{
    options(..., width=width)
    fl <- system.file(package="BiocStyle", "sty", "Bioconductor.sty")
    cat(sprintf("\\RequirePackage{%s}\n", sub(".sty$", "", fl)))
    
    setPrefix = function(x) {
      cat(sprintf("\\renewcommand{\\prefix}{%s}", x))
    }
    
    ## check whether called from knitr
    fs = sapply(sys.calls(), function(x) as.character(x)[1])
    id = pmatch("knit", fs, nomatch=0)
    
    ## knitr
    if ( id > 0 && exists("opts_chunk")) {
        if (missing(fig.path)) { 
            fig.path = opts_chunk$get("fig.path", default=TRUE)
            ## ## resolve document file name for figure name prefixing
            ## filepath = sys.frame(id)$input
            ## filename = unlist(strsplit(basename(filepath),
            ##   split=".", fixed=TRUE))
            ## filename = paste(filename[-length(filename)], collapse=".")
            ## fig.path = paste(filename, "-", sep="")
        }
        
        if ( isTRUE(short.fignames) ) setPrefix(fig.path)
        
        ## set knitr options
        opts_knit$set(latex.options.color="usenames,dvipsnames")
        opts_chunk$set(fig.path=fig.path) 
    }
    
    ## assume Sweave
    else {
      if ( isTRUE(short.fignames) ) setPrefix("\\jobname-") 
    }
    
}
