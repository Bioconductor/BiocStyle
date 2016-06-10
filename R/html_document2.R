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
    
    ## format caption titles
    x = caption_titles(x)
    
    ## cross-references
    x = resolve_refs(x)
    
    ## footnotes
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
    opts_knit = list(bookdown.internal.label = TRUE), # use labels of the form (\#label) in knitr
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
    opts_hooks = .opts_hooks)
  
  
  # Call the rmarkdown::html_document function with `template="default"` and
  # substitute the template only afterwards in order to retain some of the
  # original functionality such as mathjax, floating toc, and code folding.
  
  rmarkdown_html_document = rmarkdown::html_document(
    toc = toc,
    number_sections = number_sections,
    fig_width = fig_width,
    fig_height = fig_height,
    css = css,
    ...)
  
  template_arg = which(rmarkdown_html_document$pandoc$args == "--template") + 1L
  rmarkdown_html_document$pandoc$args[template_arg] = template
  
  rmarkdown::output_format(
    knitr = knitr,
    pandoc = NULL,
    post_processor = post_processor,
    base_format = rmarkdown_html_document)
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
  lines <- modifyLines(lines=lines, from='<script src="$navigationjs$/codefolding.js"></script>', insert=
                         sprintf('<script src="%s"></script>', file.path(resources, "html", "codefolding.js")))
  
  ## Automatic equation numbering
  lines <- modifyLines(lines=lines, from='$if(mathjax-url)$', replace=FALSE, before=TRUE, insert=c(
    '<script type="text/x-mathjax-config">',
    '  MathJax.Hub.Config({',
    '    TeX: {',
    '      TagSide: "right",',
    '      equationNumbers: {',
    '        autoNumber: "AMS"', 
    '      }',
    '    },',
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

# the following function is a copy from the RStudio's 'tufte' package
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

caption_titles = function(lines) {
  lines = readLines("/home/oles/sandbox/BiocStyle/sample_rmarkdown_BiocStyle.html.org.html")
  
  # tables: match to <caption>...</caption> lines
  regex = '(?<=^<caption>)[[:space:]]*(\\(#tab:[-[:alnum:]]+\\))?[[:space:]]*([^.]+.?)'
  idx = which(grepl(regex, lines, perl=TRUE))
  lines[idx]
  lines = gsub(regex, '\\1<span class="caption-title">\\2</span>', lines, perl=TRUE)
  
  # figures: match to figure labels preceded by '<p class="caption'
  regex = '(\\(#fig:[-[:alnum:]]+\\))[[:space:]]*([^.]+.?)'
  idx =  which(grepl(regex, lines))
  idx = idx[vapply(idx, function(i) any(grepl('^<p class="caption', lines[i-0:1])), logical(1L))]
  lines[idx] = gsub(regex, '\\1<span class="caption-title">\\2</span>', lines[idx])
  lines
}

resolve_refs = function(content) {
  
  ## Parse figure/table labels
  
  m = gregexpr('\\(#((fig|tab):[-[:alnum:]]+)\\)', content)
  labs = regmatches(content, m)
  figs = grep('^<div class="figure', content)
  
  idx = which(vapply(labs, length, integer(1L)) == 1L)
  
  newlabs = gsub('^\\(#|\\)$', '', labs[idx])
  type = ifelse(grepl('^fig:', newlabs), 'Figure', 'Table')
  
  # counters
  cntr = integer(length(type))
  for (t in c('Figure', 'Table')) {
    sel = type == t
    cntr[sel] = 1:sum(sel)
  }
  
  ref_table = setNames(cntr, newlabs)  # an array of the form c(label = number, ...)
  
  format_label = function(type, num)
    paste0('<span class="caption-label">', type, ' ', num, ': </span>')
  
  labs[idx] = 
    mapply(newlabs, idx, type, cntr, SIMPLIFY=FALSE, USE.NAMES=FALSE, 
           FUN = function(lab, i, type, num) {
             if (type == 'Figure') {
               if (any(grepl('^<p class="caption', content[i - 0:1]))) {
                 k = max(figs[figs <= i])
                 content[k] <<- paste0(content[k], sprintf('<span id="%s"></span>', lab))
                 format_label(type, num)
               } else {
                 # remove these labels, because there must be a caption on this or
                 # previous line (possible negative case: the label appears in the alt
                 # text of <img>)
                 ""
               }
             } else {
               if (any(grepl("^<caption>", content[i - 0:1]))) {
                 sprintf('<span id="%s">%s</span>', lab, format_label(type, num))
               } else {
                 ""
               }
             }
           })
  
  regmatches(content, m) = labs
  
  # remove labels in figure alt text (it will contain \ like (\#fig:label))
  content = gsub('"\\(\\\\#(fig:[-[:alnum:]]+)\\)', '"', content)
  
  
  ## Parse section labels
  
  sec_num = '^<h[1-6]><span class="header-section-number">([.A-Z0-9]+)</span>.+</h[1-6]>$'
  sec_ids = '^<div id="([^"]+)" class="section .+">$'
  
  idx = grep(sec_num, content)
  idx = idx[grepl(sec_ids, content[idx-1L])] ## make sure all sections have ids
  
  ref_table = c(ref_table, setNames(
    sub(sec_num, '\\1', content[idx]),
    sub(sec_ids, '\\1', content[idx-1L])
  ))
  
  
  ## look for @ref(label) and resolve to actual figure/table/section numbers
  m = gregexpr('(?<!\\\\)@ref\\(([-:[:alnum:]]+)\\)', content, perl = TRUE)
  refs = regmatches(content, m)
  idx = vapply(refs, length, integer(1L)) > 0L
  refs[idx] = lapply(refs[idx], ref_to_number, ref_table)
  regmatches(content, m) = refs
  
  content
}

ref_to_number = function(ref, ref_table) {
  ref = gsub('^@ref\\(|\\)$', '', ref)
  num = ref_table[ref]
  i = is.na(num)
  j = i & grepl('^eq:', ref)
  # equation labels will be replaced by \ref{eq:label}; the reason that we
  # cannot directly use \ref{} for HTML even MathJax supports it is that
  # Pandoc will remove the LaTeX command \ref{} for HTML output, and MathJax
  # needs the literal command \ref{} on the page
  i[j] = FALSE
  if (any(i)) {
    if (!isTRUE(opts$get('preview')))
      warning('Label(s) ', paste(ref[i], collapse = ', '), ' not found', call. = FALSE)
    num[i] = '<strong>??</strong>'
  }
  ifelse(j, sprintf('\\ref{%s}', ref), sprintf('<a href="#%s">%s</a>', ref, num))
}
