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

## Unit tests for the BiocStyle::html_document post-processing helpers in
## R/html_document.R.

test_modify_css_removes_pre_code_padding <- function() {
    ## rmarkdown injects 'pre code { padding: 0; }', which overrides the code
    ## block padding in bioconductor.css. Pandoc < 3 keeps its indentation,
    ## pandoc >= 3 drops it when embedding the CSS (#103).
    for (rule in list(c("pre code {", "  padding: 0;", "}"),
                      c("pre code {", "padding: 0;", "}"))) {
        x <- c("<style>", "pre code {", "padding: 10px 10px 10px 0px;", "}",
               rule, "</style>")
        y <- BiocStyle:::modify_css(x)
        checkEquals(c("<style>", "pre code {", "padding: 10px 10px 10px 0px;",
                      "}", "</style>"), y,
                    msg = paste(rule, collapse = "\\n"))
    }
}

test_modify_css_keeps_other_pre_code_rules <- function() {
    ## bootstrap's minified rule and other 'pre code' rules are left alone
    x <- c("pre code{padding:0;font-size:inherit;color:inherit}",
           "pre code {", "white-space: pre;", "}")
    checkEquals(x, BiocStyle:::modify_css(x))
}

test_caption_titles_table_nesting <- function() {
    ## the caption title <span> must close before </caption>, also when the
    ## caption has no sentence break
    ct <- BiocStyle:::caption_titles
    checkEquals(
        '<caption>(#tab:a)<span class="caption-title">kable caption</span><br></caption>',
        ct("<caption>(#tab:a) kable caption</caption>"))
    checkEquals(
        '<caption>(#tab:a)<span class="caption-title">Ends with period</span><br></caption>',
        ct("<caption>(#tab:a) Ends with period.</caption>"))
    checkEquals(
        '<caption>(#tab:a)<span class="caption-title">With <em>markup</em></span><br></caption>',
        ct("<caption>(#tab:a) With <em>markup</em></caption>"))
    checkEquals(
        '<caption>(#tab:a)<span class="caption-title">Title</span><br>More text.</caption>',
        ct("<caption>(#tab:a) Title. More text.</caption>"))
    ## caption continued on the next line
    checkEquals(
        c('<caption>(#tab:a)<span class="caption-title">Long title</span><br>',
          "continued</caption>"),
        ct(c("<caption>(#tab:a) Long title", "continued</caption>")))
}

test_caption_titles_figures <- function() {
    ct <- BiocStyle:::caption_titles
    x <- c('<p class="caption">', "(#fig:a) Figure title. Description.",
           '<p class="caption">', "(#fig:b) No period")
    checkEquals(
        c('<p class="caption">',
          '(#fig:a)<span class="caption-title">Figure title</span><br>Description.',
          '<p class="caption">',
          '(#fig:b)<span class="caption-title">No period</span><br>'),
        ct(x))
}
