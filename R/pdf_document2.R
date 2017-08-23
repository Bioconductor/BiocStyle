pdf_document2 <- function(...) {
  .Deprecated("pdf_document", old = "pdf_document2")
  pdf_document(...)
}

pdf_document <- function(toc = TRUE,
                          number_sections = TRUE,
                          fig_width = NA,
                          fig_height = NA,
                          use.unsrturl = TRUE,
                          keep_md = FALSE,
                          toc_newpage = FALSE,
                          titlecaps = TRUE,
                          ...) {
  
  ## load the package to expose macros
  require(BiocStyle, quietly = TRUE)
  
  base_format <- function(toc,
                          number_sections,
                          fig_width,
                          fig_height,
                          use.unsrturl,
                          includes,
                          keep_md,
                          toc_newpage,
                          titlecaps,
                          ...) {
    
  template <- create_latex_template(if (isTRUE(titlecaps)) NULL else "notitlecaps")
  
  head = NULL
  
  ## code chunks and code highlighting
  thm <- system.file("themes", "default.css", package = "BiocStyle")
  opts_knit$set(out.format="latex");
  head = c(head,
           "% code highlighting",
           knit_theme$get(thm)$highlight,
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
    includes$in_header = c(includes$in_header, inc$in_header)
  
  
  # pandoc options
  pandoc_args = NULL
  
  if (isTRUE(toc_newpage)) pandoc_args = c("-M", "toc-newpage")
  
  # knitr options
  knitr = merge_lists(.knitr_options(), list(
    opts_chunk = list(
      collapse = TRUE
    ),
    knit_hooks = list(
      plot = function(x, options = list()) {
        adjustwidth = NULL
        
        # adjust width for plots inserted not as floats
        if (!length(options$fig.cap) || is.na(options$fig.cap)) {
          adjustwidth = c('\\begin{adjustwidth}{\\fltoffset}{0mm}',
                          '\\end{adjustwidth}')
        }
        
        if (options$fig.env=="figure" && is.null(adjustwidth)) {
          hook_plot_md(x, options)
        }
        else {
          paste0(adjustwidth[1L],
                 hook_plot_tex(x, options),
                 adjustwidth[2L])
        }
      }
    )
  ))

  
  pdf_document_base = rmarkdown::pdf_document(
    toc = toc,
    number_sections = number_sections,
    fig_width = fig_width,
    fig_height = fig_height,
    template = template,
    includes = includes,
    pandoc_args = pandoc_args,
    ...)
  
  pdf_document_base$inherits <- "pdf_document"
  
  post_processor = function(metadata, input, output, clean, verbose) {
    lines = readUTF8(output)
    
    ## insert author affiliations
    lines <- modifyLines(lines, from='%% AUTH AFFIL %%', insert=auth_affil_latex(metadata))
    
    ## LaTeX soul hacks
    r = "(?<=\\\\texttt{)(?:\\\\{|\\\\}|[^{}])*(?=})"
    m <- gregexpr(r, lines, perl=TRUE)
    regmatches(lines, m) <- lapply(regmatches(lines, m), function(x) {
      # substitute all control spaces "\ " in \texttt by regular spaces
      x <- gsub("\\ ", " ", x, fixed=TRUE)
    })
    
    writeUTF8(lines, output)
    
    output
  }
  
  rmarkdown::output_format(knitr = knitr,
                pandoc = NULL,
                keep_md = keep_md,
                pre_processor = pre_processor,
                post_processor = post_processor,
                base_format = pdf_document_base)
  }

  bookdown::pdf_book(
    toc = toc,
    number_sections = number_sections,
    fig_width = fig_width,
    fig_height = fig_height,
    use.unsrturl = use.unsrturl,
    keep_md = keep_md,
    toc_newpage = toc_newpage,
    titlecaps = titlecaps,
    ...,
    base_format = base_format)
}


create_latex_template <- function(opts=NULL) {
  ## get and modify the default pandoc template
  
  # choose the rmarkdown template depending on pandoc version
  version <- pandoc_version()
  
  template_files <- list.files(system.file("rmd", "latex", package="rmarkdown"),
                               pattern = "\\.tex$")
  template_versions <- sub("default-?([1-9.]*).tex", "\\1", template_files)
  template_versions <- numeric_version(template_versions, strict = FALSE)
  template_versions <- sort(template_versions, decreasing = TRUE)
  
  idx <- match(TRUE, version >= template_versions)
  
  template <- if (idx > 0) sprintf("default-%s.tex", template_versions[idx]) else "default.tex"
  
  # customize the template
  lines <- readUTF8(system.file("rmd", "latex", template, package = "rmarkdown"))
  
  template <- biocTempfile("template.tex")
  
  
  # remove redundand hyperref definitions
  lines <- modifyLines(lines, from="\\usepackage[unicode=true]{hyperref}", to="\\urlstyle{same}", offset=c(-5L, 0L))
  lines <- modifyLines(lines, from="\\usepackage{hyperref}", to="\\urlstyle{same}") # >= 1.15.2
  
  # do not set links in TOC to black
  lines <- modifyLines(lines, from="\\hypersetup{linkcolor=black}")
  lines <- modifyLines(lines, from="\\hypersetup{linkcolor=$if(toccolor)$$toccolor$$else$black$endif$}") # >= 1.14
  
  # load BiocStyle after 'titling' to override title page formating, but before 
  # author specification to ensure 'hyperref' gets loaded before 'authblk'
  lines <- modifyLines(lines, from="\\usepackage{titling}", replace=FALSE, insert=c(
    "",
    loadBioconductorStyleFile(bioconductor2.sty, opts)))
  
  # use \bioctitle to capture short title for page headings
  lines <- modifyLines(lines, from="\\title{$title$}", insert="\\bioctitle[$if(shorttitle)$$shorttitle$$endif$]{$title$}")
  
  # add author affiliations
  lines <- modifyLines(lines, from="\\author{$for(author)$$author$$sep$ \\\\ $endfor$}", insert="%% AUTH AFFIL %%")
  
  # add package version number
  lines <- modifyLines(lines, from="\\end{abstract}", replace=FALSE, insert=c(
    "",
    "$if(package)$",
    "\\packageVersion{$package$}",
    "$endif$"))
  
  # remove highlighting-macros
  lines <- modifyLines(lines, from="$highlighting-macros$", offset=c(-1L, 1L))
  
  ## output TOC on a separate page
  lines <- modifyLines(lines, from="\\tableofcontents", insert=c(
    "$if(toc-newpage)$",
    "\\newpage",
    "$endif$",
    "\\tableofcontents",
    "\\newpage"))
  
  writeUTF8(lines, template)
  
  template
}
