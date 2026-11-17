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

test_pdf_document_csl_citation <- function() {
  ## Regression test for https://github.com/Bioconductor/BiocStyle/issues/94
  ##
  ## pandoc >= 3.1.8 emits \citeproc{} and \bibitem inside a list-based
  ## CSLReferences environment. The frozen pandoc-2.8 template only defined
  ## the older environment, so any PDF with a CSL bibliography failed with
  ## "Undefined control sequence \citeproc" or "Lonely \item".
  dir <- tempfile(); dir.create(dir)
  writeLines(c(
    "@book{xie2018,",
    "  title = {R Markdown: The Definitive Guide},",
    "  author = {Xie, Yihui and Allaire, J. J. and Grolemund, Garrett},",
    "  publisher = {Chapman and Hall/CRC}, year = {2018}}"
  ), file.path(dir, "refs.bib"))
  rmd <- file.path(dir, "cite.Rmd")
  writeLines(c(
    '---',
    'title: "Citation"',
    'bibliography: refs.bib',
    'output: BiocStyle::pdf_document',
    '---',
    '',
    'See @xie2018 and [@xie2018, p. 3].',
    '',
    '# References'
  ), rmd)
  out <- rmarkdown::render(rmd, quiet = TRUE)
  checkTrue(file.exists(out))
  unlink(dir, recursive = TRUE)
}

test_pdf_document_patch_latex_output <- function() {
  ## the edits BiocStyle applies to the .tex file written by pandoc
  lines <- c("\\author{A}", "%% AUTH AFFIL %%",
             "Using the \\texttt{images\\ =\\ \"lowres\"} is fine.")
  metadata <- list(author = list(list(name = "Ann Author",
                                      affiliation = "Some Institute")))
  out <- BiocStyle:::patch_latex_output(lines, metadata)
  checkTrue(!any(grepl("AUTH AFFIL", out, fixed = TRUE)),
            "affiliation placeholder replaced")
  checkTrue(any(grepl("\\affil[1]{Some Institute}", out, fixed = TRUE)),
            "affiliation inserted")
  checkTrue(any(grepl("\\texttt{images = \"lowres\"}", out, fixed = TRUE)),
            "control spaces in \\texttt replaced by regular spaces")
}

test_pdf_document_output_file <- function() {
  ## Regression test for https://github.com/Bioconductor/BiocStyle/issues/93
  ##
  ## When 'output_file' ends in '.pdf', rmarkdown::render() runs LaTeX on the
  ## .tex file before any post-processor. BiocStyle's soul fix for inline
  ## code (and the author affiliations) used to live in the post-processor, so
  ## inline code with spaces failed with "Package soul Error: Reconstruction
  ## failed." and the affiliations were never inserted.
  dir <- tempfile(); dir.create(dir)
  rmd <- file.path(dir, "inline.Rmd")
  writeLines(c(
    '---',
    'title: "Inline code"',
    'author:',
    '- name: Ann Author',
    '  affiliation: Some Institute',
    'output: BiocStyle::pdf_document',
    '---',
    '',
    'Using the `images = "lowres"` is fine, and so is',
    '`sce <- fetch_data(type = "sce")`.'
  ), rmd)

  for (keep_tex in c(TRUE, FALSE)) {
    for (output_file in list(NULL, "named.pdf", "noext")) {
      what <- sprintf("keep_tex = %s, output_file = %s", keep_tex,
                      deparse(output_file))
      out <- rmarkdown::render(
        rmd, BiocStyle::pdf_document(keep_tex = keep_tex),
        output_file = output_file, quiet = TRUE)
      checkTrue(file.exists(out), paste("PDF exists:", what))
      checkIdentical(charToRaw("%PDF"), readBin(out, "raw", 4L),
                     paste("output is a PDF:", what))
      tex <- sub("[.]pdf$", ".tex", out)
      checkIdentical(keep_tex, file.exists(tex),
                     paste(".tex file kept only with keep_tex:", what))
      if (keep_tex) {
        lines <- readLines(tex)
        checkTrue(any(grepl("\\affil[1]{Some Institute}", lines, fixed = TRUE)),
                  paste("affiliation in .tex:", what))
        checkTrue(any(grepl("\\texttt{images = \"lowres\"}", lines,
                            fixed = TRUE)),
                  paste("soul fix applied to .tex:", what))
      }
      unlink(c(out, tex))
    }
  }
  unlink(dir, recursive = TRUE)
}
