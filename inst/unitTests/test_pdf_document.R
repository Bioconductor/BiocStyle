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

test_pdf_document_uncaptioned_table <- function() {
  ## Regression test for https://github.com/Bioconductor/BiocStyle/issues/116
  ##
  ## Starting with pandoc 3.8.1, an uncaptioned table is typeset by emitting
  ## a reference to a LaTeX counter which avoids incrementing the regular
  ## 'table' counter. BiocStyle's frozen copy of the pandoc LaTeX template
  ## did not define that counter, causing compilation to fail with
  ## "! LaTeX Error: No counter '' defined." (see jgm/pandoc#11201).
  rmd <- c(
    '---',
    'title: "Uncaptioned table"',
    'output:',
    '  BiocStyle::pdf_document:',
    '    latex_engine: lualatex',
    '---',
    '',
    'An uncaptioned Markdown pipe table, with no R code involved at all:',
    '',
    '| File type | Suggested function     |',
    '| ----------| ---------------------- |',
    '| CSV       | `readr::read_csv()`    |',
    '| TSV       | `readr::read_tsv()`    |',
    '| SPSS      | `haven::read_spss()`   |',
    '| xls/xlsx  | `readxl::read_excel()` |'
  )
  filename <- tempfile(fileext = ".Rmd")
  writeLines(rmd, filename)
  output_file <- tempfile(fileext = ".pdf")
  rmarkdown::render(filename, output_file = output_file, quiet = TRUE)
  checkTrue(file.exists(output_file))
  unlink(c(filename, output_file))
}
