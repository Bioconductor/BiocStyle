pdf_document_old <- function(toc = TRUE,
                         number_sections = TRUE,
                         use.unsrturl = TRUE,
                         includes,
                         ...) {
  .Deprecated("pdf_document", old = "pdf_document_old")
  
  ## load the package to expose macros
  require(BiocStyle, quietly = TRUE)
  
  # get the locations of resource files located within the package
  template <- file.path(resources, "tex", "template.tex")
  
  head = sprintf("\\RequirePackage{%s}\n", sub(".sty$", "", bioconductor.sty))
  
  if (use.unsrturl) {
    bst <- file.path(resources, "tex", "unsrturl")
    head = c(head, sprintf("\\AtBeginDocument{\\bibliographystyle{%s}}\n", bst))
  }
  
  # dump to a header file which will be included in the template
  header = tempfile("", fileext = ".tex")
  writeLines(head, header)
  inc = rmarkdown::includes(in_header = header)
  
  if ( missing(includes) )
    includes = inc
  else
    includes$in_header = c(includes$in_header, inc$in_header)
  
  # call the base pdf_document function
  rmarkdown::output_format(knitr = rmarkdown::knitr_options(opts_chunk = list(collapse=TRUE)),
                           pandoc = NULL,
                           base_format = rmarkdown::pdf_document(
                            toc = toc,
                            number_sections = number_sections,
                            template = template,
                            includes = includes,
                            ...)
  )
}
