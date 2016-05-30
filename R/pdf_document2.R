pdf_document2 <- function(toc = TRUE,
                          number_sections = TRUE,
                          fig_width = NA,
                          fig_height = NA,
                          fig_caption = TRUE,
                          use.unsrturl = TRUE,
                          includes,
                          keep_md = FALSE,
                          keep_tex = FALSE,
                          toc_newpage = FALSE,
                          titlecaps = TRUE,
                          ...) {
  
  ## load the package to expose macros
  require(BiocStyle, quietly = TRUE)
  
  template <- create_latex_template()
  
  head = NULL
  
  ## code chunks and code highlighting
  thm <- system.file("themes", "default.css", package = "BiocStyle")
  head = c(head,
           "% code highlighting",
           knitr:::theme_to_header_latex(thm)$highlight,
           readLines(file.path(resources, "tex", "highlighting-macros.def")))
  
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
    includes$in_header = c(includes$in_header, inc)
  
  
  # pandoc options
  pandoc_args = NULL
  
  if (isTRUE(toc_newpage)) pandoc_args = c("-M", "toc-newpage")
  
  # knitr options
  knitr = list(
    opts_knit = list(width = .width()),
    opts_chunk = list(collapse=TRUE, fig.scap=NA),
    knit_hooks = list(
      plot = function(x, options = list()) {
        adjustwidth = NULL
        
        # adjust width for plots inserted not as floats
        if (!length(options$fig.cap) || is.na(options$fig.cap)) {
          adjustwidth = c('\\begin{adjustwidth}{\\fltoffset}{0mm}',
                          '\\end{adjustwidth}')
        }
        
        if (options$fig.env=="figure" && is.null(adjustwidth)) {
          knitr::hook_plot_md(x, options)
        }
        else {
          paste0(adjustwidth[1L],
                 knitr::hook_plot_tex(x, options),
                 adjustwidth[2L])
        }
      }),
    opts_hooks = .opts_hooks,
    opts_template = NULL
  )
  
  # LEGACY CODE: when rmarkdown 0.9.6 is released add dependency in DESCRIPTION and remove the following lines
  if( packageVersion("rmarkdown") < package_version("0.9.6") ) {
    if (is.na(fig_width)) fig_width = 7.5
    if (is.na(fig_height)) fig_height = 5.0
  }
  
  # call the base pdf_document function
  rmarkdown::output_format(knitr = knitr,
                           pandoc = NULL,
                           keep_md = keep_md,
                           clean_supporting = !keep_tex,
                           base_format = rmarkdown::pdf_document(
                             toc = toc,
                             number_sections = number_sections,
                             fig_width = fig_width,
                             fig_height = fig_height,
                             fig_caption = fig_caption,
                             template = template,
                             includes = includes,
                             keep_tex = keep_tex,
                             pandoc_args = pandoc_args,
                             ...)
  )
}


create_latex_template <- function() {
  ## get and modify the default pandoc template
  
  # choose the rmarkdown template depending on pandoc version
  version <- rmarkdown::pandoc_version()
  
  template_files <- list.files(system.file("rmd", "latex", package="rmarkdown"),
                               pattern = "\\.tex$")
  template_versions <- sub("default-?([1-9.]*).tex", "\\1", template_files)
  template_versions <- numeric_version(template_versions, strict = FALSE)
  template_versions <- sort(template_versions, decreasing = TRUE)
  
  idx <- match(TRUE, version >= template_versions)
  
  template <- if (idx > 0) sprintf("default-%s.tex", template_versions[idx]) else "default.tex"
  
  # customize the template
  lines <- readLines(system.file("rmd", "latex", template, package = "rmarkdown"))
  
  template <- file.path(tempdir(), "newtemplate.tex")
  
  
  
  # remove redundand hyperref definitions
  lines <- modifyLines(lines=lines, from="\\usepackage[unicode=true]{hyperref}", to="\\urlstyle{same}", offset=c(-5L, 0L))
  lines <- modifyLines(lines=lines, from="\\usepackage{hyperref}", to="\\urlstyle{same}") # >= 1.15.2
  
  # do not set links in TOC to black
  lines <- modifyLines(lines=lines, from="\\hypersetup{linkcolor=black}")
  lines <- modifyLines(lines=lines, from="\\hypersetup{linkcolor=$if(toccolor)$$toccolor$$else$black$endif$}") # >= 1.14
  
  # load BiocStyle after 'titling' to override title page formating, but before 
  # author specification to ensure 'hyperref' gets loaded before 'authblk'
  lines <- modifyLines(lines=lines, from="\\usepackage{titling}", replace=FALSE, c("", loadBioconductorStyleFile("notitlecaps")))
  
  # add author affiliations
  lines <- modifyLines(lines=lines, from="\\author{$for(author)$$author$$sep$ \\\\ $endfor$}", c(
    "$for(author)$",
    "$if(author.name)$  \\author{$author.name$$if(author.email)$\\thanks{\\ttfamily$author.email$}$endif$}$else$  \\author{$author$}$endif$",
    "$if(author.affiliation)$  \\affil{$author.affiliation$}$endif$$endfor$"))
  
  # add package version number
  lines <- modifyLines(lines=lines, from="\\end{abstract}", replace=FALSE, c(
    "",
                                    "$if(package)$",
                                      "\\packageVersion{$package$}",
                                      "$endif$"))
  
  # remove highlighting-macros
  lines <- modifyLines(lines=lines, from="$highlighting-macros$", offset=c(-1L, 1L))
  
  ## output TOC on a separate page
  lines <- modifyLines(lines=lines, from="\\tableofcontents", c(
    "$if(toc-newpage)$",
    "\\newpage",
                                                                  "$endif$",
                                                                  "\\tableofcontents",
                                                                  "\\newpage"))
  
  writeLines(lines, template)
  
  template
}
