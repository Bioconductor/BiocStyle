loadBioconductorStyleFile <- function(titlecaps) {
  ## set LaTeX options
  opts = NULL
  if (!isTRUE(titlecaps))
    opts = c(opts, "notitlecaps")
  sprintf("\\RequirePackage[%s]{%s}",
        paste(opts, collapse = ","),
        sub(".sty$", "2", bioconductor.sty))
}

latex2 <-
    function(..., width=68, titlecaps = TRUE, short.fignames=FALSE, fig.path,
             error=FALSE, use.unsrturl=TRUE) {
    options(..., width=width)
    cat(
      loadBioconductorStyleFile(titlecaps = titlecaps),
      if (use.unsrturl) {
        bst <- file.path(resources, "tex", "unsrturl")
        sprintf("\\AtBeginDocument{\\bibliographystyle{%s}}", bst)
      }, sep = "\n")
 
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
        knitr::opts_chunk$set(fig.path=fig.path, error=error, crop=TRUE)
        
        # reset figure dimensions to detect values set by user
        knitr::opts_chunk$set(fig.width=NA, fig.height=NA)
        
        knitr::opts_hooks$set(
          # options fig.small and fig.wide have precedance over fig.env
          fig.small = function(options) {
            if (isTRUE(options$fig.small)) {
              options$fig.env = "smallfigure"
            }
            options
          },
          fig.wide = function(options) {
            if (isTRUE(options$fig.wide)) {
              options$fig.env = "figure*"
            }
            options
          },
          # set default plot dimensions if user provided no values
          fig.width = function(options) {
            if (is.na(options$fig.width)) {
              options$fig.width = switch(options$fig.env,
                                         "smallfigure" = 5,
                                         "figure*" = 10,
                                         "figure" = 7.5,
                                         7) # knitr default
            }
            options
          },
          fig.height = function(options) { 
            if ( is.na(options$fig.height) ){
              options$fig.height = 5
            }
            options
          }
        )
        
        # set hooks for special plot output
        knitr::knit_hooks$set(
          plot = function(x, options = list()) {
            adjustwidth = NULL
            
            # adjust width for plots inserted not as floats
            if (!length(options$fig.cap) || is.na(options$fig.cap)) {
              adjustwidth = c('\\begin{adjustwidth}{\\fltoffset}{0mm}',
                              '\\end{adjustwidth}')
            }
            
            # call the default knitr hook as defined in render_latex()
            paste0('\\end{kframe}',
                   adjustwidth[1L],
                   knitr::hook_plot_tex(x, options),
                   adjustwidth[2L],
                   '\\begin{kframe}')
          },
          
          ## remove figure margins with pdfcrop
          crop = knitr::hook_pdfcrop,
          
          ## proper aspect ratio of plots
          eval = function(before, options) {
            if (before) {
              #par(mar=c(4,4,0.5,0.5))
            }
          }
        )
        
        ## code highlighting
        knitr::opts_knit$set(out.format = "latex")
        thm <- system.file("themes", "default.css", package = "BiocStyle")
        knitr::knit_theme$set(thm)
        
        # suppress \definecolor{shadecolor} in knitrout environment
        knitr::opts_chunk$set(background = NA) 
    }
    
    ## assume Sweave
    else {
      if ( isTRUE(short.fignames) ) setPrefix("\\jobname-")
      cat("\\usepackage[noae, nogin]{Sweave}")
    }
    
}
