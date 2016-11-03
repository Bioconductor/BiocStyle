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
          fig.path = knitr::opts_chunk$get("fig.path", default=TRUE)
        } else {
          knitr::opts_chunk$set(fig.path=fig.path)
        }
        
        if ( isTRUE(short.fignames) ) setPrefix(fig.path)
      
        ## set knitr options
        knitr::opts_knit$set(latex.options.color="usenames,dvipsnames")
        
        knitr::opts_chunk$set(.opts_chunk)
        
        knitr::opts_hooks$set(.opts_hooks)
        
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
        thm <- system.file("themes", "default.css", package = "BiocStyle")
        knitr::knit_theme$set(thm)
        
        ## reset header$framed which by default includes the content of knitr.sty
        knitr::set_header(framed = "\\newenvironment{knitrout}{}{} % an empty environment to be redefined in TeX")
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
        knitr::current_input()
      } else {
        tryCatch(dynGet("file"), error = function(e) NULL) # TODO: might need to be more specific here
      }
      
      if (!is.null(src)) {
        fontsize = tryCatch({
          lines = readLines(src)
          documentclass = grep("^[:blank:]*\\\\documentclass", lines, value = TRUE)[1L]
          sub(".+(1[0-2]pt).+","\\1", documentclass)
        }, error = function(e) NULL)
      }
      
      width = .width(fontsize, knitr)
    }
    
    options(..., width = width)
    }

.width = function(fontsize = "10pt", knitr = TRUE, default = 70L) {
  w = if (is.null(fontsize)) 
    default
  else
    switch(fontsize, default,
           "12pt" = 58L,
           "11pt" = 64L,
           "10pt" = 70L,
           "9pt"  = 78L,
           "8pt"  = 88L)
  
  # knitr output is usually commented out
  if (knitr)
    w = w - knitr:::comment_length(knitr::opts_chunk$get("comment"))
  
  w
}

.opts_chunk = list(
  error = FALSE,
  fig.scap = NA, # disable default short caption extraction
  crop = TRUE,
  background = NA, # suppress \definecolor{shadecolor} in knitrout environment 
  fig.width = NA, # reset figure dimensions to detect values set by user
  fig.height = NA
)

.opts_hooks = list(
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
  # Set default plot dimensions if user provided no values
  # Important: the hooks are processed in the order they are defined here
  # regardless of the order of options in the code chunk header
  fig.width = function(options) {
    if (is.na(options$fig.width)) {
      options$fig.width = switch(options$fig.env, 7,# fallback to knitr default
                                 "smallfigure" = 5,
                                 "figure*" = 10,
                                 "figure" = 7.5)
      # override 'fig.height' if 'fig.asp' is set (as plain knitr does)
      if (is.numeric(options$fig.asp))
        options$fig.height = options$fig.width * options$fig.asp
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
