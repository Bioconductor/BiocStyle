html_document <- function(css, theme = NULL, ...) {
  
  # append any user-provided CSS files
  css = 
    if ( missing(css) )
      bioc.css
    else
      c(bioc.css, css)
  
  # call the base html_document function
  rmarkdown::html_document(css = css, theme = theme, ...)
}
