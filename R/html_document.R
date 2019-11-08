#' Use Bioconductor style to format R Markdown HTML output
#' 
#' Format for converting from R Markdown to an Bioconductor HTML document.
#' 
#' \code{BiocStyle::html_document} format extends the
#' \code{\link[rmarkdown]{html_document}} format. See the
#' \href{http://rmarkdown.rstudio.com/html_document_format.html}{online
#' documentation} and the package vignette "Authoring R Markdown Vignettes" for
#' additional details on using the format,
#' 
#' @param toc logical(1), \code{TRUE} to include a table of contents in the
#' output
#' @param number_sections logical(1), \code{TRUE} to number section headings
#' @param fig_width numeric(1), default width (in inches) for figures
#' @param fig_height numeric(1), default width (in inches) for figures
#' @param self_contained numeric(1), \code{TRUE} to produce a standalone HTML
#' file with no external dependencies, using data: URIs to incorporate the
#' contents of linked scripts, stylesheets, images, and videos. Note that even
#' for self contained documents MathJax is still loaded externally (this is
#' necessary because of it's size).
#' @param css character, one or more css files to include
#' @param pandoc_args character, additional command line options to pass to
#' pandoc
#' @param \dots Additional arguments passed to
#' \code{\link[rmarkdown]{html_document}}
#' @param titlecaps logical(1), \code{TRUE} to use the emphasize the first
#' sentence in figure and table captions as title
#' @return R Markdown output format to pass to \code{\link[rmarkdown]{render}}
#' @author Andrzej Oleś <andrzej.oles@@embl.de>, 2014-2017
#' @seealso \code{\link[BiocStyle]{pdf_document}},
#' \code{\link[BiocStyle]{md_document}}
#' @examples
#' 
#' \dontrun{
#' 
#' # simple invocation
#' render("input.Rmd", BiocStyle::html_document())
#' 
#' # specify options
#' render("input.Rmd", BiocStyle::html_document(toc = FALSE))
#' }
#' 
#' @export html_document
html_document <- function(toc = TRUE,
                          number_sections = TRUE,
                          fig_width = NA,
                          fig_height = NA,
                          self_contained = TRUE,
                          css = NULL,
                          pandoc_args = NULL,
                          ...,
                          ## BiocStyle specific arguments:
                          titlecaps = TRUE) {
  
  ## load the package to expose macros
  require(BiocStyle, quietly = TRUE)
  
  ## customize the default rmarkdown template
  template <- create_html_template()
  
  # append any user-provided CSS files
  css <- c(bioconductor.css, css)
  
  post_processor = function(metadata, input, output, clean, verbose) {
    x = readUTF8(output)
    
    ## insert author affiliations
    x <- modifyLines(x, from='<!-- AUTH AFFIL -->', insert=auth_affil_html(metadata))
    
    ## format caption titles
    if (isTRUE(titlecaps))
      x = caption_titles(x)
    
    ## replace footnotes with sidenotes
    x = process_footnotes(x)
    
    ## wrap tables in a container
    x = process_tables(x)
    
    writeUTF8(x, output)
    output
  }
  
  # knitr options
  knitr = merge_lists(.knitr_options(), list(
    knit_hooks = list(
      plot = function(x, options = list()) {
        out.extra = switch(options$fig.env, NULL,
                           "smallfigure" = 'class="smallfigure"',
                           "figure*" = 'class="widefigure"',
                           "figure" = NULL)
        options$out.extra = paste(options$out.extra, out.extra)
        hook_plot_md(x, options)
      }
    )
  ))
  
  # mask section numbering in bookdown in order to get a global rather than
  # per-section numbering of figures/tables
  
  number_sections_override <- number_sections
  
  base_format <- function(..., number_sections) {
    rmarkdown::html_document(..., number_sections = number_sections_override)
  }
  
  # call the rmarkdown::html_document function with `template="default"` and
  # substitute the template only afterwards in order to retain some of the
  # original functionality such as mathjax, floating toc, and code folding.
  config <- output_format(
    knitr = knitr,
    pandoc = NULL,
    clean_supporting = self_contained,
    pre_processor = pre_processor,
    post_processor = post_processor,
    base_format = bookdown::html_document2(
      base_format = base_format,
      toc = toc,
      number_sections = FALSE,
      fig_width = fig_width,
      fig_height = fig_height,
      self_contained = self_contained,
      css = css,
      ...)
  )
  
  ## override some default pandoc args; we use this low-level approach rather 
  ## than passing them in 'pandoc_args' to 'rmarkdown::html_document' because 
  ## rmarkdown just concatenates base and overlay argument lists which does not 
  ## allow for substitution
  pandoc_args = c(pandoc_args, c("--template", template))
  
  arg_names <- c("--email-obfuscation", "--template")
  arg_names <- arg_names[arg_names %in% pandoc_args]
  
  idx = match(arg_names, pandoc_args)
  
  ## substitute arguments
  config$pandoc$args [
    match(arg_names, config$pandoc$args) + 1L 
    ] <- pandoc_args[idx + 1L]
  
  ## append the rest
  config$pandoc$args <- c(config$pandoc$args, pandoc_args[-c(idx, idx+1L)])
  
  config
}

# modify the default rmarkdown template
create_html_template <- function() {
  lines <- readUTF8(system.file("rmd", "h", "default.html", package = "rmarkdown"))
  
  template <- biocTempfile("template.html")
  
  ## placeholder for author affiliation block which is inserted during postprocessing
  lines <- modifyLines(lines, from='$for(author)$', to='$endfor$', insert='<!-- AUTH AFFIL -->')
  
  lines <- modifyLines(lines, from='<div class="abstract">', to='</div>', insert=c(
    '<h4 class="abstract">Abstract</h4>',
    '$abstract$',
    '$endif$',
    '$if(package)$',
    '<h4 class="package">Package</h4>',
    '<p>$package$</p>'))
  
  lines <- modifyLines(lines, from='<div id="$idprefix$TOC">', replace=FALSE, before=TRUE, insert="<h1>Contents</h1>")
  
  ## modify some inline CSS
  lines <- modifyLines(lines, from='^\\.toc-content \\{', to = '\\}', fixed=FALSE)
  
  lines <- modifyLines(lines, from='^\\code \\{', to = '\\}', fixed=FALSE)
  
  for (i in 1:2) 
    lines <- modifyLines(lines, from='^  pre:not\\(\\[class\\]\\) \\{', to = '\\}', fixed=FALSE)
  
  lines <- modifyLines(lines=lines, from='^\\.main-container \\{', to = '\\}', fixed=FALSE, offset=c(1L, -1L), insert=c(
    '  max-width: 828px;',
    '  margin-left: auto;',
    '  margin-right: auto;'))
  
  lines <- modifyLines(lines, from='^div\\.tocify \\{', to = '\\}', fixed=FALSE, offset=c(1L, -1L), insert=c(
    '  width: 20%;',
    '  max-width: 246px;',
    '  max-height: 85%;'))
  
  ## use the modified code folding script
  lines <- modifyLines(lines=lines, from='<script src="$navigationjs$/codefolding.js"></script>', insert=
                         sprintf('<script src="%s"></script>', file.path(resources, "html", "codefolding.js")))
  
  ## Automatic equation numbering
  lines <- modifyLines(lines=lines, from='$if(mathjax-url)$', replace=FALSE, before=TRUE, insert=c(
    '<script type="text/x-mathjax-config">',
    '  MathJax.Hub.Config({',
    # '    TeX: {',
    # '      TagSide: "right",',
    # '      equationNumbers: {',
    # '        autoNumber: "AMS"', 
    # '      }',
    # '    },',
    '    "HTML-CSS": {',
    '      styles: {',
    '        ".MathJax_Display": {',
    '           "text-align": "center",',
    '           padding: "0px 150px 0px 65px",',
    '           margin: "0px 0px 0.5em"',
    '        },',
    '      }',
    '    }',
    '  });',
    '</script>'))
  
  lines <- modifyLines(lines = lines, from = "</head>", replace = FALSE, before = TRUE, 
                       insert = c(
                           '<script>',
                           '    function toggle_visibility(id1) {',
                           '        var e = document.getElementById(id1);',
                           '        e.style.display = ((e.style.display!="none") ? "none" : "block");',
                           '    }',
                           '</script>'))
  
  writeUTF8(lines, template)
  
  template
}

caption_titles = function(lines) {
  regex_template = '%s(\\(#%s:[-/[:alnum:]]+\\))[[:space:]]*(.*?)([[:space:]]*((\\.[[:space:]])|(\\.$)|($)))'
  replacement = '\\1<span class="caption-title">\\2</span><br>'
  
  # tables: match to <caption>...</caption> lines
  regex = sprintf(regex_template, '(?<=^<caption>)[[:space:]]*', 'tab')
  lines = gsub(regex, replacement, lines, perl=TRUE)
  
  # figures: match to figure labels preceded by '<p class="caption'
  regex = sprintf(regex_template, '', 'fig')
  idx =  which(grepl(regex, lines))
  idx = idx[vapply(idx, function(i) any(grepl('^<p class="caption', lines[i-0:1])), logical(1L))]
  lines[idx] = gsub(regex, replacement, lines[idx])
  
  lines
}

process_footnotes = function(lines) {
  fn_label = paste0(knitr::opts_knit$get("rmarkdown.pandoc.id_prefix"), "fn")
  
  ## match to footnotes block
  i = which(lines == '<div class="footnotes">')
  if (length(i) == 0L)
    return(lines)
  j = which(lines == '</div>')
  j = min(j[j > i])
  
  ## extract footnotes and their ids
  r = sprintf('<li id="%s([0-9]+)"><p>(.+)<a href="#%sref\\1"([^>]*)>.</a></p></li>', fn_label, fn_label)
  s = paste(lines[i:j], collapse = '\n')
  fns = unlist(regmatches(s, gregexpr(r, s)))
  ids = as.integer(gsub(r, '\\1', fns))
  fns = gsub(r, '\\2', fns)
  
  # remove footnotes at the bottom
  lines = lines[-(i:j)]
  
  # replace footnotes with sidenotes
  cls = if (pandoc_available('2.0')) 'footnote-ref' else 'footnoteRef'
  for (i in seq_along(ids)) {
    id = ids[i]
    lines = sub(
      sprintf(
        '<a href="#%s%d" class="%s" id="%sref%d"><sup>%d</sup></a>',
        fn_label, id, cls, fn_label, id, id),
      sprintf(paste0(
        '<label for="sidenote-%d" class="sidenote-number">%d</label>',
        '<label for="sidenote-%d" class="margin-toggle" onclick="toggle_visibility(\'sidenote-%d\')">%d</label>',
        '<span class="sidenote" id="sidenote-%d"><span class="sidenote-number">%d</span> %s</span>'
        ), id, id, id, id, id, id, id, fns[i]),
      lines, fixed=TRUE)
  }
  lines
}

## We wrap tables in a <div> so they can fill the screen on mobile
## but also scroll if they overflow horizontally
process_tables = function(lines) {
  lines = gsub("<table", "<div class='horizontal-scroll'><table", lines, fixed = TRUE)
  lines = gsub("</table>", "</table></div>", lines, fixed = TRUE)
  lines
}
