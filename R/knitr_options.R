# a list of knitr options as returned by rmarkdown::knitr_options
.knitr_options = function() {
  list(
    opts_knit = list(
      width = .width()
    ),
    opts_chunk = .opts_chunk,
    knit_hooks = list(
      # remove figure margins
      crop = hook_pdfcrop
    ),
    opts_hooks = .opts_hooks,
    opts_template = NULL
  )
}

.opts_chunk = list(
  error = FALSE,
  fig.scap = NA, # disable default short caption extraction
  crop = TRUE
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
                                 "figure" = 8)
      # override 'fig.height' if 'fig.asp' is set (as plain knitr does)
      if (is.numeric(options$fig.asp))
        options$fig.height = options$fig.width * options$fig.asp
    }
    
    # re-evaluate code from knitr:::fix_options which is called before the hook
    if ( !is.null(options$out.width) && is.na(options$out.width) )
      options$out.width = "100%"
    
    options
  },
  fig.height = function(options) {
    if ( is.na(options$fig.height) ){
      options$fig.height = 5
    }
    
    options
  }
)
