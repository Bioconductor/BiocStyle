
## create an example HTML file with a footnote to be converted
input <- "This is a test sentence^[here is a footnote]"
tf_md <- tempfile(fileext = ".md")
writeLines(input, con = tf_md)
tf_html <- tempfile(fileext = ".html")
rmarkdown::pandoc_convert(input = tf_md, to = "html", output = tf_html, options = c("--wrap=preserve"))

## convert footnotes to sidenotes
lines <- BiocStyle:::process_footnotes(BiocStyle:::readUTF8(tf_html))

checkTrue(
  grepl("sidenote", lines), msg = "Sidenote not found in HTML output"
)
checkTrue(
  !grepl("footnote", lines), msg = "Footnote found in HTML output"
)
