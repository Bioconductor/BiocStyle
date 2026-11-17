test_html_document_keep_md <- function() {
  ## Regression test for https://github.com/Bioconductor/BiocStyle/issues/106
  ##
  ## html_document() called rmarkdown::output_format() without 'keep_md', so
  ## its default FALSE overrode the value passed to the base format.
  checkTrue(BiocStyle::html_document(keep_md = TRUE)$keep_md)
  checkTrue(!BiocStyle::html_document()$keep_md)

  dir <- tempfile(); dir.create(dir)
  rmd <- file.path(dir, "keep.Rmd")
  writeLines(c('---', 'title: "Keep MD"', 'output:',
               '  BiocStyle::html_document:', '    keep_md: true', '---', '',
               '# Section', '', '```{r}', 'plot(1:10)', '```'), rmd)
  out <- rmarkdown::render(rmd, quiet = TRUE)
  checkTrue(file.exists(out))
  md <- file.path(dir, "keep.md")
  checkTrue(file.exists(md), ".md file kept")
  ## the figure referenced by the Markdown file is kept as well
  img <- regmatches(readLines(md), regexpr('keep_files/[^"]+[.]png', readLines(md)))
  checkEquals(1L, length(img))
  checkTrue(file.exists(file.path(dir, img)), "figure referenced by .md exists")
  unlink(dir, recursive = TRUE)
}
