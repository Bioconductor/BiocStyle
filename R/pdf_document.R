#' Use Bioconductor style to format R Markdown PDF output
#' 
#' This function sets the Bioconductor style in PDF documents rendered using R
#' Markdown v2.
#' 
#' @inheritParams html_document
#' 
#' @param includes Named list of additional content to include within the
#' document (typically created using the \code{\link{includes}} function).
#' @param \dots Additional arguments passed to
#' \code{\link[rmarkdown]{pdf_document}}.
#' @param toc_newpage logical(1), \code{TRUE} to start the table of contents on
#' a new page.
#' @param use_unsrturl logical(1), indicating that the \sQuote{unsrturl} style
#' will be used (\code{\\bibliographystyle} command \emph{not} required).
#' @param relative_path logical(1), copy supporting LaTeX files and use
#' relative paths rather than absolute paths to system files.
#' @return R Markdown output format to pass to \code{\link[rmarkdown]{render}}.
#' @author Andrzej Oleś <andrzej.oles@@embl.de>, 2014-2017
#' @seealso \code{\link[BiocStyle]{html_document}},
#' \code{\link[BiocStyle]{md_document}}
#' @keywords manip
#' @examples
#' 
#' \dontrun{
#' 
#' # simple invocation
#' render("input.Rmd", BiocStyle::pdf_document())
#' 
#' # specify an option for latex engine
#' render("input.Rmd", BiocStyle::pdf_document(toc = FALSE))
#' }
#' 
#' @export
pdf_document <- function(toc = TRUE,
                         number_sections = TRUE,
                         fig_width = NA,
                         fig_height = NA,
                         includes = NULL,
                         ...,
                         ## BiocStyle specific arguments:
                         titlecaps = TRUE,
                         toc_newpage = FALSE,
                         use_unsrturl = TRUE,
                         relative_path = FALSE) {
  
  ## load the package to expose macros
  require(BiocStyle, quietly = TRUE)
  
  relative_path <- isTRUE(relative_path)
  use_unsrturl <- isTRUE(use_unsrturl)
  
  sty <- bioconductor.sty
  bst <- if (use_unsrturl) file.path(resources, "tex", "unsrturl.bst") else NULL
  
  template_files <- c(sty=sty, bst=bst)
  
  pre_knit <- intermediates_generator <- NULL
  
  if ( relative_path ) {
    template_files <- copyResource(template_files, getwd())

    # use `pre_knit` to copy template files as this seems to be the only
    # function which is aware of the original input file name passed to `render`
    pre_knit <- function(input, ...) {
      file.copy(template_files, dirname(normalizePath(input)))
    }

    # run when document is rendered to a different directory than the input file
    intermediates_generator <- function(original_input, encoding, intermediates_dir) {
      file.copy(template_files, normalizePath(intermediates_dir))
      basename(template_files)
    }

  }
  
  template <- create_latex_template(if (isTRUE(titlecaps)) NULL else "notitlecaps", template_files[["sty"]])
  
  head = NULL
  
  ## code chunks and code highlighting
  thm <- system.file("themes", "default.css", package = "BiocStyle")
  opts_knit$set(out.format="latex");
  head = c(head,
           "% code highlighting",
           knit_theme$get(thm)$highlight,
           readLines(file.path(resources, "tex", "highlighting-macros.def")))
  
  if (use_unsrturl)
    head = c(head, sprintf("\\AtBeginDocument{\\bibliographystyle{%s}}\n",  sub(".bst$", "", template_files[["bst"]])))
  
  # dump to a header file which will be included in the template
  header = tempfile("", fileext = ".tex")
  writeLines(head, header)
  
  inc = rmarkdown::includes(in_header = header)
  
  if ( is.null(includes) )
    includes = inc
  else
    includes$in_header = c(includes$in_header, inc$in_header)
  
  # pandoc options
  pandoc_args = NULL
  
  if (isTRUE(toc_newpage))
    pandoc_args = c("--variable", "toc-newpage")
  
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
    ),
    opts_hooks = list(
      # respect fig.pos, see https://github.com/rstudio/rmarkdown/issues/1012
      fig.pos = function(options) {
        if (is.null(options$out.extra)) {
          options$out.extra = ''
        }
        options
      }
    )
  ))
  
  post_processor = function(metadata, input, output, clean, verbose) {
    lines = readUTF8(output)
    
    ## insert author affiliations
    lines <- modifyLines(lines, from='%% AUTH AFFIL %%', insert=auth_affil_latex(metadata))
    
    ## LaTeX soul hacks
    r = "(?<=\\\\texttt{)((?:\\\\{|\\\\}|[^{}]|{(?1)})*)(?=})"
    m <- gregexpr(r, lines, perl=TRUE)
    regmatches(lines, m) <- lapply(regmatches(lines, m), function(x) {
      # substitute all control spaces "\ " in \texttt by regular spaces
      x <- gsub("\\ ", " ", x, fixed=TRUE)
    })
    
    writeUTF8(lines, output)
    
    output
  }
  
  config <- output_format(
    knitr = knitr,
    pandoc = list(args = pandoc_args),
    pre_knit = pre_knit,
    pre_processor = pre_processor,
    post_processor = post_processor,
    intermediates_generator = intermediates_generator,
    # use bookdown::pdf_book for the added capability of cross-referencing
    base_format = bookdown::pdf_book(
      toc = toc,
      number_sections = number_sections,
      fig_width = fig_width,
      fig_height = fig_height,
      template = "default",
      includes = includes,
      base_format = rmarkdown::pdf_document,
      ...)
  )
  
  ## override the default document template which is used in order to retain
  ## some original template-dependent rmarkdown functionality
  config$pandoc$args[ match("--template", config$pandoc$args) + 1L ] <- template
  
  ## remove the obsolete default 'geometry' pandoc variable after it has beed
  ## added by the default 'pdf_pre_processor' defined in rmarkdown::pdf_document
  pre = config$pre_processor
  config$pre_processor = function(metadata, input_file, runtime, knit_meta, files_dir, output_dir) {
    args = if ( is.function(pre) )
      pre(metadata, input_file, runtime, knit_meta, files_dir, output_dir)
    else
      pre
    
    pos = match("geometry:margin=1in", args)
    if (!is.na(pos))
      args = args[-c(pos-1L, pos)]
    
    args
  }
  
  config
}

create_latex_template <- function(opts=NULL, sty=bioconductor.sty) {
  ## get and modify the default pandoc template
  
  # choose the rmarkdown template depending on pandoc version
  version <- pandoc_version()
  
  template_files <- list.files(system.file("rmd", "latex", package="rmarkdown"),
                               pattern = "\\.tex$")
  template_versions <- sub("default-?([0-9.]*).tex", "\\1", template_files)
  template_versions <- numeric_version(template_versions, strict = FALSE)
  template_versions <- sort(template_versions, decreasing = TRUE)
  
  idx <- match(TRUE, version >= template_versions, nomatch = 0L)
  
  template <- if (idx > 0L) sprintf("default-%s.tex", template_versions[idx]) else "default.tex"
  
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
    loadBioconductorStyleFile(sty, opts)))
  
  # use \bioctitle to capture short title for page headings
  lines <- modifyLines(lines, from="\\title{$title$}", insert="\\bioctitle[$if(shorttitle)$$shorttitle$$endif$]{$title$}")
  
  # add author affiliations
  lines <- modifyLines(lines, from="\\author{$for(author)$$author$$sep$ \\\\ $endfor$}", insert="%% AUTH AFFIL %%")
  
  # add package version number
  lines <- modifyLines(lines, from="\\end{abstract}", offset=1L, replace=FALSE, insert=c(
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
