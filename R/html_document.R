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
  .Deprecated("html_document", old = "html_document_old")
  
  ## load the package to expose macros
  require(BiocStyle, quietly = TRUE)
  
  # append any user-provided CSS files
  css <- c(bioconductor.css, css)

  # build pandoc args
  args <- c("--standalone")

  # use section divs
  args <- c(args, "--section-divs")

  # table of contents
  args <- c(args, rmarkdown::pandoc_toc_args(toc, toc_depth))

  # template path and assets
  args <- c(args, "--template", rmarkdown::pandoc_path_arg(file.path(resources, "html", "template.html")))
  
  # highlight
  args <- c(args, rmarkdown:::pandoc_html_highlight_args("default", highlight))
  
  # add highlight.js html_dependency if required
  if (!is.null(highlight) && (highlight %in% c("default", "textmate"))) {
    extra_dependencies <- append(extra_dependencies, list(
      htmltools::htmlDependency(
        "highlightjs",
        version = "1.1",
        src = rmarkdown:::rmarkdown_system_file("rmd/h/highlightjs-1.1"),
        script = "highlight.js",
        stylesheet = paste0(highlight, ".css")
      )
    ))
  }
  
  # numbered sections
  if (number_sections)
    args <- c(args, "--number-sections")

  # additional css
  for (css_file in css)
    args <- c(args, "--css", rmarkdown::pandoc_path_arg(css_file))

  # pre-processor for arguments that may depend on the name of the
  # the input file (e.g. ones that need to copy supporting files)
  pre_processor <- function(metadata, input_file, runtime, knit_meta, files_dir,
                            output_dir) {

    # extra args
    args <- c()

    # content includes (we do this here so that user include-in-header content
    # goes after dependency generated content). make the paths absolute if
    # making a Shiny document so we can resolve them even if rendering
    # elsewhere.
    args <- c(args, rmarkdown::includes_to_pandoc_args(includes,
                      filter = if (identical(runtime, "shiny"))
                        rmarkdown:::normalize_path
                      else
                        identity))

    # return additional args
    args
  }

  # return format
  rmarkdown::output_format(
    knitr = rmarkdown::knitr_options_html(fig_width, fig_height, fig_retina, keep_md, dev),
    pandoc = rmarkdown::pandoc_options(to = "html",
                            from = rmarkdown:::from_rmarkdown(fig_caption, md_extensions),
                            args = args),
    keep_md = keep_md,
    clean_supporting = self_contained,
    pre_processor = pre_processor,
    base_format = rmarkdown::html_document_base(smart = smart, theme = NULL,
                                     self_contained = self_contained,
                                     lib_dir = lib_dir, mathjax = mathjax,
                                     template = "default",
                                     pandoc_args = pandoc_args,
                                     extra_dependencies = extra_dependencies,
                                     ...)
  )
}
