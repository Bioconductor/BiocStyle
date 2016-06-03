## This function uses a modified copy of the original rmarkdown::html_document 
## function. The reason for it is that we want to use custom template but trick
## rmarkdown into thinking that it uses its default template, which allows to
## retain some of the original functionality such as mathjax, floating toc, and
## code folding.

html_document2 <- function(toc = TRUE,
                           number_sections = TRUE,
                           fig_width = NA,
                           fig_height = NA,
                           fig_retina = NULL,
                           css = NULL,
                           ...) {
  
  ## load the package to expose macros
  require(BiocStyle, quietly = TRUE)
  
  ## customize the default rmarkdown template
  template <- create_html_template()
  
  # append any user-provided CSS files
  css <- c(bioconductor2.css, css)
  
  post_processor = function(metadata, input, output, clean, verbose) {
    x = readUTF8(output)
    footnotes = parse_footnotes(x)
    notes = footnotes$items
    # replace footnotes with sidenotes
    for (i in seq_along(notes)) {
      num = sprintf(
        '<a href="#fn%d" class="footnoteRef" id="fnref%d"><sup>%d</sup></a>',
        i, i, i
      )
      con = sprintf(paste0(
        '<label for="sidenote-%d" class="margin-toggle sidenote-number">%d</label>',
        '<input type="checkbox" id="sidenote-%d" class="margin-toggle">',
        '<span class="sidenote"><span class="sidenote-number">%d</span> %s</span>'
      ), i, i, i, i, notes[i])
      x = gsub(num, con, x, fixed=TRUE)
    }
    # remove footnotes at the bottom
    if (length(footnotes$range)) x = x[-footnotes$range]
    
    writeUTF8(x, output)
    output
  }
  
  # knitr options
  knitr = rmarkdown::knitr_options(
    opts_chunk = list(),
    knit_hooks = list(
      plot = function(x, options = list()) {
        out.extra = switch(options$fig.env, NULL,
                           "smallfigure" = 'class="smallfigure"',
                           "figure*" = 'class="widefigure"',
                           "figure" = NULL)
        options$out.extra = paste(options$out.extra, out.extra)
        knitr::hook_plot_md(x, options)
      }),
    opts_hooks = c(.opts_hooks,
                   fig.cap = function(options) {
                     options
                   }),
  )
  
  rmarkdown::output_format(
    knitr = knitr,
    pandoc = NULL,
    post_processor = post_processor,
    base_format = rmarkdown_html_document(toc = toc,
                                          number_sections = number_sections,
                                          fig_width = fig_width,
                                          fig_height = fig_height,
                                          template = template,
                                          #fig_retina = fig_retina,
                                          css = css,
                                          ...))
}

# modify the default rmarkdown template
create_html_template <- function() {
  lines <- readUTF8(system.file("rmd", "h", "default.html", package = "rmarkdown"))
  
  template <- biocTempfile("template.html")
  
  lines <- modifyLines(lines, from='<div class="abstract">', to='</div>', insert=c(
    '<h4 class="abstract">Abstract</h4>',
    '$abstract$',
    '$endif$',
    '$if(package)$',
    '<h4 class="package">Package: <span style="font-weight: normal">$package$</span></h4>'))
  
  lines <- modifyLines(lines, from='<div id="$idprefix$TOC">', replace=FALSE, before=TRUE, insert="<h1>Contents</h1>")
  
  ## modify some inline CSS
  lines <- modifyLines(lines, from='^\\.toc-content \\{', to = '\\}', fixed=FALSE)
  
  lines <- modifyLines(lines, from='^\\code \\{', to = '\\}', fixed=FALSE)
  
  for (i in 1:2) 
    lines <- modifyLines(lines, from='^  pre:not\\(\\[class\\]\\) \\{', to = '\\}', fixed=FALSE)
  
  lines <- modifyLines(lines=lines, from='^\\.main-container \\{', to = '\\}', fixed=FALSE, offset=c(1L, -1L), insert=c(
    '  max-width: 768px;',
    '  margin-left: auto;',
    '  margin-right: auto;'))
  
  lines <- modifyLines(lines, from='^div\\.tocify \\{', to = '\\}', fixed=FALSE, offset=c(1L, -1L), insert=c(
    '  width: 20%;',
    '  max-width: 226px;',
    '  max-height: 85%;'))
  
  ## use the modified code folding script
  lines <- modifyLines(lines=lines, from='<script src="$navigationjs$/codefolding.js"></script>',
                       insert=sprintf('<script src="%s"></script>', 
                                      file.path(resources, "html", "codefolding.js")))
  
  writeUTF8(lines, template)
  
  template
}

# the following function is copied from the RStudio's 'tufte' package
parse_footnotes = function(x) {
  i = which(x == '<div class="footnotes">')
  if (length(i) == 0) return(list(items = character(), range = integer()))
  j = which(x == '</div>')
  j = min(j[j > i])
  n = length(x)
  r = '<li id="fn([0-9]+)"><p>(.+)<a href="#fnref\\1">.</a></p></li>'
  list(
    items = gsub(r, '\\2', grep(r, x[i:n], value = TRUE)),
    range = i:j
  )
}
