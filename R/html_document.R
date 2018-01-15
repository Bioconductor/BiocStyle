## this function uses a modified copy of the original rmarkdown::html_document 
## function; the reason for using this approach rather than calling 
## html_document is that we want to use custom template but trick rmarkdown into
## thinking that it uses its default template which allows to retain original
## mathjax functionality

html_document_old <- function(toc = TRUE,
                          toc_depth = 3,
                          number_sections = TRUE,
                          fig_width = 7,
                          fig_height = 5,
                          fig_retina = NULL,
                          fig_caption = FALSE,
                          dev = 'png',
                          smart = TRUE,
                          self_contained = TRUE,
                          highlight = "default",
                          mathjax = "default",
                          extra_dependencies = NULL,
                          css = NULL,
                          includes = NULL,
                          keep_md = FALSE,
                          lib_dir = NULL,
                          md_extensions = NULL,
                          pandoc_args = NULL,
                          ...) {
  .defunct("html_document", old = "html_document_old")
}
