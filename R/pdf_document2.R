pdf_document2 <- function(toc = TRUE,
                          number_sections = TRUE,
                          fig_width = 7.5,
                          fig_height = 5.0,
                          fig_caption = TRUE,
                          use.unsrturl = TRUE,
                          includes,
                          keep_md = FALSE,
                          keep_tex = FALSE,
                          toc_newpage = FALSE,
                          titlecaps = TRUE,
                          width = 68,
                          ...) {
  
  ## load the package to expose macros
  require(BiocStyle, quietly = TRUE)
  
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
  
  template <- file.path(tempdir(), template)
  
  substituteLines = function (lines, from, to, insert = NULL, replace = TRUE, from.offset = 0L, to.offset = 0L, ...) {
    from = grep(from, lines, fixed=TRUE)
    # exit if nothing found
    if (length(from)==0)
      return(lines)
    
    if (missing(to)) {
      to = from
    } else {
      to = grep(to, lines, fixed=TRUE)
    }
    
    # exit if nothing found
    if (length(to)==0)
      return(lines)
    
    from = from + from.offset
    to = to + to.offset
    
    if (isTRUE(replace)) { # substitute lines from-to
      c(lines[1:(from-1L)], insert, lines[(to+1L):length(lines)])
    } else { # append after to
      c(lines[1:to], insert, lines[(to+1L):length(lines)])
    }
  }
  
  # remove redundand hyperref definitions
  lines <- substituteLines(lines, "\\usepackage[unicode=true]{hyperref}", "\\urlstyle{same}", from.offset = -5L)
  lines <- substituteLines(lines, "\\usepackage{hyperref}", "\\urlstyle{same}") # >= 1.15.2
  
  # do not set links in TOC to black
  lines <- substituteLines(lines, "\\hypersetup{linkcolor=black}")
  lines <- substituteLines(lines, "\\hypersetup{linkcolor=$if(toccolor)$$toccolor$$else$black$endif$}") # >= 1.14
  
  # load BiocStyle after 'titling' to override title page formating, but before 
  # author specification to ensure 'hyperref' gets loaded before 'authblk'
  lines <- substituteLines(lines, "\\usepackage{titling}", replace = FALSE, to.offset = 1L,
                           insert = c(loadBioconductorStyleFile(titlecaps = titlecaps), ""))
  
  # add author affiliations
  lines <- substituteLines(lines, "\\author{$for(author)$$author$$sep$ \\\\ $endfor$}",
                           insert = c("$for(author)$",
                                      "$if(author.name)$  \\author{$author.name$}$else$  \\author{$author$}$endif$",
                                      "$if(author.affiliation)$  \\affil{$author.affiliation$}$endif$$endfor$"))
  
  # add package version number
  lines <- substituteLines(lines, "\\end{abstract}", replace = FALSE,
                           insert = c("",
                                      "$if(package)$",
                                      "\\package{$package$}",
                                      "$endif$"))
  
  # remove highlighting-macros
  lines <- substituteLines(lines, "$highlighting-macros$", from.offset = -1L, to.offset = 1L)
  
  ## output TOC on a separate page
  lines <- substituteLines(lines, "\\tableofcontents", insert = c("$if(toc-newpage)$",
                                                                  "\\newpage",
                                                                  "$endif$",
                                                                  "\\tableofcontents",
                                                                  "\\newpage"))
  
  writeLines(lines, template)
  
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
  knitr = rmarkdown::knitr_options(
    opts_knit = list(width = width),
    opts_chunk = list(collapse=TRUE),
    knit_hooks = list(
      plot = function(x, options = list()) {
        # adjust width for plots which are not inserted as floats
        if (!length(options$fig.cap) || is.na(options$fig.cap)) {
          paste0('\\begin{adjustwidth}{\\fltoffset}{0mm}',
                 knitr::hook_plot_tex(x, options),
                 '\\end{adjustwidth}')
        } else {
          knitr::hook_plot_md(x, options)
        }
      })
  )
  
  # pandoc options
  pandoc_args = NULL
  
  if (isTRUE(toc_newpage)) pandoc_args = c("-M", "toc-newpage")
  
  # knitr options
  knitr = rmarkdown::knitr_options(
    opts_chunk = list(collapse=TRUE),
    knit_hooks = list(
      plot = function(x, options = list()) {
        # adjust width for plots which are not inserted as floats
        if (!length(options$fig.cap) || is.na(options$fig.cap)) {
          paste0('\\begin{adjustwidth}{\\fltoffset}{0mm}',
                 knitr::hook_plot_tex(x, options),
                '\\end{adjustwidth}')
        } else {
          knitr::hook_plot_md(x, options)
        }
      })
  )
  
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
