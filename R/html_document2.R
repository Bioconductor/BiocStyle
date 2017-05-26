html_document2 <- function(toc = TRUE,
                           number_sections = TRUE,
                           fig_width = NA,
                           fig_height = NA,
                           fig_retina = NULL,
                           self_contained = TRUE,
                           css = NULL,
                           pandoc_args = NULL,
                           ...) {
  
  ## load the package to expose macros
  require(BiocStyle, quietly = TRUE)
  
  base_format <- function(toc = TRUE,
                          number_sections = TRUE,
                          fig_width = NA,
                          fig_height = NA,
                          fig_retina = NULL,
                          self_contained,
                          css = NULL,
                          ...) {
  
  ## customize the default rmarkdown template
  template <- create_html_template()
  
  # append any user-provided CSS files
  css <- c(bioconductor2.css, css)
  
  post_processor = function(metadata, input, output, clean, verbose) {
    x = readUTF8(output)
  
    ## insert author affiliations
    x <- modifyLines(x, from='<!-- AUTH AFFIL -->', insert=auth_affil_html(metadata))
    
    ## format caption titles
    x = caption_titles(x)
    
    ## replace footnotes with sidenotes
    x = process_footnotes(x)
    
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
  
  # Call the rmarkdown::html_document function with `template="default"` and
  # substitute the template only afterwards in order to retain some of the
  # original functionality such as mathjax, floating toc, and code folding.
  
  rmarkdown_html_document = rmarkdown::html_document(
    toc = toc,
    number_sections = TRUE,
    fig_width = fig_width,
    fig_height = fig_height,
    self_contained = self_contained,
    css = css,
    ...)
  
  ## override some default pandoc args; we use this low-level approach rather 
  ## than passing them in 'pandoc_args' to 'rmarkdown::html_document' because 
  ## rmarkdown just concatenates base and overlay argument lists which does not 
  ## allow for substitution
  pandoc_args = c(pandoc_args, c("--template", template))
  
  arg_names <- c("--email-obfuscation", "--template")
  arg_names <- arg_names[arg_names %in% pandoc_args]
  
  idx = match(arg_names, pandoc_args)
  
  ## substitute arguments
  rmarkdown_html_document$pandoc$args [
    match(arg_names, rmarkdown_html_document$pandoc$args) + 1L
  ] <- pandoc_args [idx + 1L]
  
  ## append the rest
  rmarkdown_html_document$pandoc$args <- 
    c(rmarkdown_html_document$pandoc$args, pandoc_args[-c(idx, idx+1L)])
  
  
  rmarkdown::output_format(
    knitr = knitr,
    pandoc = NULL,
    clean_supporting = self_contained,
    pre_processor = pre_processor,
    post_processor = post_processor,
    base_format = rmarkdown_html_document)
  }

  bookdown::html_document2(
    base_format = base_format,
    toc = toc,
    number_sections = FALSE,
    fig_width = fig_width,
    fig_height = fig_height,
    fig_retina = fig_retina,
    self_contained = self_contained,
    css = css,
    ...
  )
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
  
  writeUTF8(lines, template)
  
  template
}

caption_titles = function(lines) {
  # tables: match to <caption>...</caption> lines
  regex = '(?<=^<caption>)[[:space:]]*(\\(#tab:[-/[:alnum:]]+\\))?[[:space:]]*([^.]+).?'
  lines = gsub(regex, '\\1<span class="caption-title">\\2</span><br>', lines, perl=TRUE)
  
  # figures: match to figure labels preceded by '<p class="caption'
  regex = '(\\(#fig:[-/[:alnum:]]+\\))[[:space:]]*([^.]+).?'
  idx =  which(grepl(regex, lines))
  idx = idx[vapply(idx, function(i) any(grepl('^<p class="caption', lines[i-0:1])), logical(1L))]
  lines[idx] = gsub(regex, '\\1<span class="caption-title">\\2</span><br>', lines[idx])
  lines
}

process_footnotes = function(lines) {
  ## match to footnotes block
  i = which(lines == '<div class="footnotes">')
  if (length(i) == 0L)
    return(lines)
  j = which(lines == '</div>')
  j = min(j[j > i])
  
  ## extract footnotes and their ids
  r = '<li id="fn([0-9]+)"><p>(.+)<a href="#fnref\\1">.</a></p></li>'
  fns = grep(r, lines[i:j], value=TRUE)
  ids = as.integer(gsub(r, '\\1', fns))
  fns = gsub(r, '\\2', fns)
  
  # remove footnotes at the bottom
  lines = lines[-(i:j)]
  
  # replace footnotes with sidenotes
  for (i in seq_along(ids)) {
    id = ids[i]
    fn = fns[i]
    lines = sub(
      sprintf(
        '<a href="#fn%d" class="footnoteRef" id="fnref%d"><sup>%d</sup></a>',
        id, id, id),
      sprintf(paste0(
        '<label for="sidenote-%d" class="margin-toggle sidenote-number">%d</label>',
        '<input type="checkbox" id="sidenote-%d" class="margin-toggle">',
        '<span class="sidenote"><span class="sidenote-number">%d</span> %s</span>'
        ), id, id, id, id, fn),
      lines, fixed=TRUE)
  }
  lines
}
