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
