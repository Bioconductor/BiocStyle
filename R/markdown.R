markdown = function(css.files, self.contained = TRUE) {
  ## load the package to expose macros
  require(BiocStyle, quietly = TRUE)
  
  ## check version of markdown renderer
  # rmarkdown.version = if ( as.character(sys.call(1))[1] == "rmarkdown::render" ) 2L else 1L

  ## in case of rmarkdown v1insert directly into document body to circumvent
  ## issess with preview in R Studio
  
  insert.into.body = (
    is.null(knitr::opts_knit$get("rmarkdown.version")) && 
    as.character(sys.call(1))[1] != "tools::buildVignettes" )
  
  bioc.css = system.file(package = 'BiocStyle', 'css', 'bioconductor.css')
  
  if (insert.into.body)
    cat(.paste(bioc.css, scoped = TRUE))
  else
    options(markdown.HTML.stylesheet = bioc.css)
    
  if ( !missing(css.files) ) {
    # fail save
    css.files = css.files[file.exists(css.files)]

    if ( length(css.files) > 0 ) {
      if (insert.into.body)
        cat(.paste(css.files, scoped = TRUE))      
      else
        options(markdown.HTML.header = 
          if(isTRUE(self.contained))
            # insert the contents of the CSS files into the HTML document
            sapply(css.files, .paste)
          else          
            # insert relative links to the .css files
            sprintf('<link rel="stylesheet" type="text/css" href="./%s"/>', css.files)
          )
    }
  }
  
  invisible()
}

.paste = function(x, scoped = FALSE)
  paste(if (scoped) '<style type="text/css" scoped>' else '<style type="text/css">',
        paste(readLines(x), collapse = '\n'),
        '</style>', sep = "\n")

## macro definitions

Biocpkg = function(pkg) {
  sprintf('[%s](http://bioconductor.org/packages/release/bioc/html/%s.html)', pkg, pkg)
}

Biocannopkg = function(pkg) {
  sprintf('[%s](http://bioconductor.org/packages/release/data/annotation/html/%s.html)', pkg, pkg)
}

Biocexptpkg = function(pkg) {
  sprintf('[%s](http://bioconductor.org/packages/release/data/experiment/html/%s.html)', pkg, pkg)
}

CRANpkg = function(pkg) {
  sprintf('[%s](http://cran.fhcrc.org/web/packages/%s/index.html)', pkg, pkg)
}
