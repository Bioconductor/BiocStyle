loadBioconductorStyleFile <- function(opts=NULL) {
  sprintf("\\RequirePackage[%s]{%s}",
        paste(opts, collapse = ","),
        sub(".sty$", "2", bioconductor.sty))
}

latex2 <- function(..., width, titlecaps = TRUE, short.fignames=FALSE, fig.path,
                   use.unsrturl=TRUE) {
    cat(
      loadBioconductorStyleFile(if (isTRUE(titlecaps)) NULL else "notitlecaps"),
      if (use.unsrturl) {
        bst <- file.path(resources, "tex", "unsrturl")
        sprintf("\\AtBeginDocument{\\bibliographystyle{%s}}", bst)
      }, sep = "\n")
 
    setPrefix = function(x) {
      cat(sprintf("\\renewcommand{\\prefix}[1]{%s#1}", x))
    }
    
    ## check whether called from knitr
    fs = sapply(sys.calls(), function(x) as.character(x)[1])
    knitr = any(grepl("knit", fs, fixed = TRUE))
    
    ## knitr
    if (knitr) {
        if (missing(fig.path)) {
          fig.path = opts_chunk$get("fig.path", default=TRUE)
        } else {
          opts_chunk$set(fig.path=fig.path)
        }
        
        if ( isTRUE(short.fignames) ) setPrefix(fig.path)
      
        ## set knitr options
        opts_knit$set(latex.options.color="usenames,dvipsnames")
        
        opts_chunk$set(
          c(.opts_chunk,
            background = NA, # suppress \definecolor{shadecolor} in knitrout environment 
            fig.width = NA, # reset figure dimensions to detect values set by user
            fig.height = NA)
        )
        
        opts_hooks$set(.opts_hooks)
        
        # set hooks for special plot output
        knit_hooks$set(
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
                   hook_plot_tex(x, options),
                   adjustwidth[2L],
                   '\\begin{kframe}')
          },
          
          ## remove figure margins with pdfcrop
          crop = hook_pdfcrop,
          
          ## proper aspect ratio of plots
          eval = function(before, options) {
            if (before) {
              #par(mar=c(4,4,0.5,0.5))
            }
          }
        )
        
        ## code highlighting
        thm <- system.file("themes", "default.css", package = "BiocStyle")
        knit_theme$set(thm)
        
        ## reset header$framed which by default includes the content of knitr.sty
        set_header(framed = "\\newenvironment{knitrout}{}{} % an empty environment to be redefined in TeX")
    }
    
    ## assume Sweave
    else {
      if ( isTRUE(short.fignames) ) setPrefix("\\jobname-")
      cat("\\usepackage[noae, nogin]{Sweave}")
    }
    
    ## line width of output code chunks
    if (missing(width)) {
      fontsize = NULL
      
      # try to resolve the optimal width based on document font size setting
      src = if (knitr) {
        current_input()
      } else {
        tryCatch(dynGet("file"), error = function(e) NULL) # TODO: might need to be more specific here
      }
      
      if (!is.null(src)) {
        fontsize = tryCatch({
          lines = readLines(src)
          documentclass = grep("^[:blank:]*\\\\documentclass", lines, value = TRUE)[1L]
          sub(".+(1?[01289]pt).+","\\1", documentclass)
        }, error = function(e) NULL)
      }
      
      width = .width(fontsize, knitr)
    }
    
    options(..., width = width)
    }

.width = function(fontsize = "10pt", knitr = TRUE, default = 80L) {
  w = if (is.null(fontsize)) 
    default
  else
    switch(fontsize, default,
           "12pt" = 66L,
           "11pt" = 73L,
           "10pt" = 80L,
           "9pt"  = 89L,
           "8pt"  = 100L)
  
  # knitr output is usually commented out
  if (knitr) {
    com = opts_chunk$get("comment")
    if (!is.null(com) && !is.na(com) && nzchar(com))
      w = w - (nchar(com) + 1L)
  }
  
  w
}
