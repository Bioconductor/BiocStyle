markdown = function(css.files, self.contained = TRUE) {
  ## load the package to expose macros
  require(BiocStyle, quietly = TRUE)
  
  options(markdown.HTML.stylesheet = system.file(package = 'BiocStyle', 'css', 'bioconductor.css'))
  
  ## check whether called from rmarkdown::render
  fs = sapply(sys.calls(), function(x) as.character(x)[1])
  id = pmatch("render", fs, nomatch = 0L)
  rmarkdown = (id > 0L)
  
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
