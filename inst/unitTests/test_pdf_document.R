test_pdf_document <- function() {
  filename <- rmarkdown::draft(tempfile(fileext = ".Rmd"), edit = FALSE,
                               template="pdf_document", package = "BiocStyle")
  lines <- BiocStyle:::readUTF8(filename)
  lines <- BiocStyle:::modifyLines(lines, from = "title:", replace = FALSE,
                       insert='subtitle: "Vignette Subtitle"')
  BiocStyle:::writeUTF8(lines, filename)
  rmarkdown::render(filename)
  unlink(filename)
}

test_pdf_document_restores_out_format <- function() {
  ## Regression test for https://github.com/Bioconductor/BiocStyle/pull/108
  ##
  ## BiocStyle::pdf_document() temporarily sets the global knitr option
  ## 'out.format' to "latex" in order to look up the syntax highlighting
  ## theme, but used to leave it set afterwards. This corrupted the knitr
  ## state for anything rendered later in the same R session, e.g. pkgdown
  ## building several articles (one of which uses BiocStyle::pdf_document)
  ## within a single process, which itself relies on 'out.format' being
  ## "html".
  out_format_before <- knitr::opts_knit$get("out.format")
  on.exit(knitr::opts_knit$set(out.format = out_format_before))

  knitr::opts_knit$set(out.format = "html")
  invisible(BiocStyle::pdf_document())
  checkIdentical(knitr::opts_knit$get("out.format"), "html")
}
