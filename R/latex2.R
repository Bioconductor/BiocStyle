latex2 <-
    function(..., width=69, short.fignames=FALSE, fig.path,
             error=FALSE, use.unsrturl=TRUE)
{
    options(..., width=width)
    cat(
      sprintf("\\RequirePackage{%s}", sub(".sty$", "2", bioconductor.sty)),
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
        
        # reset figure dimensions to detect user input values
        knitr::opts_chunk$set(fig.width=NULL, fig.height=NULL)
        
        # set hooks for special plot output
        knitr::knit_hooks$set(
          plot = function(x, options = list()) {
          
            # determine caption (if any)
            caption <- ifelse(is.null(options$fig.cap), 
                              "",
                              paste(" \\caption{", options$fig.cap, "}\n", sep = ""))
            
            # determine figure type
            if (isTRUE(options$fig.small)) 
              figtype <- "smallfigure"
            else if (isTRUE(options$fig.wide))
              figtype <- "figure*"
            else
              figtype <- "figure"
            
            # return the latex
            paste0("\\end{kframe}\n", 
                  sprintf('\\begin{%s}\n \\includegraphics{%s}\n%s\\end{%s}\n', figtype, x, caption, figtype),
                  "\\begin{kframe}")
          },
        crop = hook_pdfcrop,
        ## hook for setting device dimensions
        eval = function(before, options) {
                    if (before) {
                      ## set defaults in case no values provided
                      #cat("before\n")
                      if ( is.null(options$fig.width) && is.null(options$fig.height) ) {
                        ## use symmetric horizontal vs. vertical margins
                        #do.call("par", list(mar=c(4,4,0.5,0.5)), envir = sys.frame(-3))
                        
                        if ( isTRUE(options$fig.small) ) {
                          options$fig.width = 5
                          options$fig.height = 5
                        }
                        else if (isTRUE(options$fig.wide)) {
                          options$fig.width = 10
                          options$fig.height = 5
                        }
                        else {
                          options$fig.width = 7.5
                          options$fig.height = 5
                        }
                        env = sys.frame(-3)
                        assign("options", options, envir = env)
                       
#                         eval(expression({
#                           ## switch off the device previously opened
#                           #dev.off()
#                           ## open a new device
#                           if (chunk_device(options$fig.width[1L], options$fig.height[1L], options$fig.keep != 'none',
#                                                            options$dev, options$dev.args, options$dpi)) {
#                             # preserve par() settings from the last code chunk
#                               if (keep.pars <- opts_knit$get('global.par'))
#                                 par(opts_knit$get('global.pars'))
#                               showtext(options$fig.showtext)  # showtext support
#                               dv = dev.cur()
#                               on.exit({
#                                 if (keep.pars) opts_knit$set(global.pars = par(no.readonly = TRUE))
#                                 dev.off(dv)
#                               }, add = TRUE)
#                           }
#                           }), env)
#                         
                        par(mar=c(4,4,0.5,0.5))
                      }
                    }
                  }
#         fig.wide = function(before, options, envir) {
#           if (before) {
#             browser()
#             opts = options
#             opts$fig.width=10
#             opts$fig.height=5
#             #opts_current$set(fig.width=5, fig.height=10)
#             #assign("options", opts, envir = parent.frame(3)) 
#             assign("options", opts, envir = sys.frame(-3))
#             #cat(paste("before", opts_current$get("fig.width")))
#           } else{
#             cat(paste("after", opts_current$get("fig.width")))
#           }
#         }
        )
        
        
        
        
    }
    
    ## assume Sweave
    else {
      if ( isTRUE(short.fignames) ) setPrefix("\\jobname-") 
    }
    
    }

pkg_ver2 <- function(pkg) {
  paste(pkg, packageVersion(pkg), sep="}{")
}
