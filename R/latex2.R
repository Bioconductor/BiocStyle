loadBioconductorStyleFile <- function(file, opts=NULL) {
  sprintf("\\RequirePackage[%s]{%s}",
        paste(opts, collapse = ","),
        sub(".sty$", "", file))
}

copyResource <- function(file, dir) {
  filename <- basename(file)
  file.copy(file, file.path(dir, filename))
  filename
}

latex2 <- function(...) {
  .Deprecated("latex", old="latex2")
  latex(...)
}

latex <- function(..., width, titlecaps = TRUE, short.fignames=FALSE, fig.path,
                   use.unsrturl=TRUE, relative.path = FALSE) {
  
  sty <- file.path(resources, "tex", "Bioconductor2.sty")
  if ( isTRUE(relative.path) )
    sty <- copyResource(sty, getwd())
      
  cat(loadBioconductorStyleFile(sty, if (isTRUE(titlecaps)) NULL else "notitlecaps"), sep="\n")
  
  if ( isTRUE(use.unsrturl) ) {
    bst <- file.path(resources, "tex", "unsrturl.bst")
    if ( isTRUE(relative.path) )
      bst <- copyResource(bst, getwd())
    cat(sprintf("\\AtBeginDocument{\\bibliographystyle{%s}}", sub(".bst$", "", bst)), sep="\n")
  }
 
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
            adjustwidth1 = adjustwidth2 = NULL
            
            # adjust width of plots not inserted as floats
            if (!length(options$fig.cap) || is.na(options$fig.cap)) {
              # multiple plots: begin at 1, end at fig.num
              fig.cur = max(options$fig.cur, 1L)
              fig.num = max(options$fig.num, 1L)
              standalone = options$fig.show != 'hold'
              
              # open adjustwidth env if this plot is standalone or first in set
              if (standalone || fig.cur==1L)
                adjustwidth1 = '\\begin{adjustwidth}{\\fltoffset}{0mm}'
              # close adjustwidth env if this plot is standalone or last in set
              if (standalone || fig.cur==fig.num)
                adjustwidth2 = '\\end{adjustwidth}'
            }
            
            # call the default knitr hook as defined in render_latex()
            paste0('\\end{kframe}',
                   adjustwidth1,
                   hook_plot_tex(x, options),
                   adjustwidth2,
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
          lines = readLines(src, warn=FALSE)
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
