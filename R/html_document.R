html_document <- function(css, theme = NULL, ...) {
  
  # append any user-provided CSS files
  css = 
    if ( missing(css) )
      .bioconductor.css
    else
      c(.bioconductor.css, css)
  
  # call the base html_document function
  rmarkdown::html_document(css = css, theme = theme, ...)
}
