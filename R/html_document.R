html_document <- function(css, ...) {
  # get the location of the CSS file located within the package
  bioc.css = system.file(package = 'BiocStyle', 'css', 'bioconductor.css')
  
  # append any user-provided CSS files
  css = 
    if ( missing(css) )
      bioc.css
    else
      c(bioc.css, css)
  
  # call the base html_document function
  rmarkdown::html_document(css = css, ...)
}
