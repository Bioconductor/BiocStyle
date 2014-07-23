markdown = function(css.files, self.contained = TRUE) {
  options(markdown.HTML.stylesheet = system.file(package = 'BiocStyle', 'css', 'bioconductor.css'))
  
  if ( !missing(css.files) ) {
    # fail save
    css.files = css.files[file.exists(css.files)]

    if ( length(css.files) > 0 ) {
    
      options(markdown.HTML.header = 
        if(isTRUE(self.contained))
          # insert the contents of the CSS files into the HTML document
          sapply(css.files, function(x) {
            paste('<style type="text/css">', paste(readLines(x), collapse = '\n'), '</style>', sep = "\n")
          })
        else          
          # insert relative links to the .css files
          sprintf('<link rel="stylesheet" type="text/css" href="./%s"/>', css.files)
        )
    }
  }
  
  invisible()
}
