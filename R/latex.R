latex_old <- function(..., width=90, short.fignames=FALSE, fig.path,
                      error=FALSE, use.unsrturl=TRUE) {
    .Deprecated("latex", old="latex_old")
    
    options(..., width=width)
    cat(sprintf("\\RequirePackage{%s}\n\n",
                sub(".sty$", "", bioconductor.sty)))
    
    if (use.unsrturl) {
        bst <- file.path(resources, "tex", "unsrturl")
        cat(sprintf("\\AtBeginDocument{\\bibliographystyle{%s}}\n", bst))
    }
    
    setPrefix = function(x) {
      cat(sprintf("\\renewcommand{\\prefix}[1]{%s#1}", x))
    }
    
    ## check whether called from knitr
    fs = sapply(sys.calls(), function(x) as.character(x)[1])
    id = pmatch("knit", fs, nomatch=0)
    
    ## knitr
    if ( id > 0 && exists("opts_chunk")) {
        if (missing(fig.path)) { 
            fig.path = knitr::opts_chunk$get("fig.path", default=TRUE)
            ## ## resolve document file name for figure name prefixing
            ## filepath = sys.frame(id)$input
            ## filename = unlist(strsplit(basename(filepath),
            ##   split=".", fixed=TRUE))
            ## filename = paste(filename[-length(filename)], collapse=".")
            ## fig.path = paste(filename, "-", sep="")
        }
        
        if ( isTRUE(short.fignames) ) setPrefix(fig.path)
        
        ## set knitr options
        knitr::opts_knit$set(latex.options.color="usenames,dvipsnames")
        knitr::opts_chunk$set(fig.path=fig.path, error=error)
    }
    
    ## assume Sweave
    else {
      if ( isTRUE(short.fignames) ) setPrefix("\\jobname-") 
    }
    
}
