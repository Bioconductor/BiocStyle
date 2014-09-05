pdf_document <- function(toc = TRUE,
                         number_sections = TRUE,
                         use.unsrturl = TRUE,
                         includes,
                         ...) {
  
  # get the locations of resource files located within the package
  template <- system.file(package = "BiocStyle", "templates", "bioconductor.tex")
  
  head = sprintf("\\RequirePackage{%s}\n", sub(".sty$", "", .bioconductor.sty))
  
  if (use.unsrturl) {
    bst <- file.path(system.file(package="BiocStyle", "sty"), "unsrturl")
    head = c(head, sprintf("\\AtBeginDocument{\\bibliographystyle{%s}}\n", bst))
  }
  
  # dump to a header file which will be included in the template
  header = tempfile("", fileext = ".tex")
  writeLines(head, header)
  inc = rmarkdown::includes(in_header = header)
  
  if ( missing(includes) )
    includes = inc
  else
    includes$in_header = c(includes$in_header, inc)
  
  # call the base pdf_document function
  rmarkdown::pdf_document(toc = toc,
                          number_sections = number_sections,
                          template = template,
                          includes = includes,
                          ...)
}
