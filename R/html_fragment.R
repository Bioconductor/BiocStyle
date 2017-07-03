html_fragment <- function(...,
                          toc = TRUE,
                          number_sections = TRUE,
                          theme = NULL,
                          highlight = NULL) {
  
  ## enable BiocStyle macros
  require(BiocStyle, quietly = TRUE)
  
  knitr <- knitr_options(opts_knit = list(width = 80L), 
                         # remove figure margins
                         opts_chunk = list(crop = TRUE),
                         knit_hooks = list(crop = hook_pdfcrop))
  
  post_processor <- function(metadata, input, output, clean, verbose) {
    lines <- readUTF8(output)
    
    ## move all headers one level down (for proper formatting when embedded in the website)
    lines <- process_headers(lines)
    
    ## add author affiliations
    lines <- modifyLines(lines, from='<!-- AUTH AFFIL -->', insert=auth_affil_html(metadata))
    
    writeUTF8(lines, output)
    output
  }
  
  config <- rmarkdown::output_format(
    knitr = knitr,
    pandoc = NULL,
    post_processor = post_processor,
    base_format = bookdown::html_document2(
      ...,
      toc = toc,
      number_sections = number_sections,
      theme = theme,
      highlight = highlight
    ))
  
  ## swap template afterwards in order to retain original mathjax functionality
  template <- system.file("resources", "fragment.html", package="BiocStyle")
  idx <- which(config$pandoc$args=="--template") + 1L
  config$pandoc$args[idx] <- template
  
  ## one needs to run the post processor processing headers before bookdown, but
  ## process captions only after bookdown
  
  post = config$post_processor
  
  config$post_processor <- function(metadata, input, output, clean, verbose) {
    if (is.function(post)) output = post(metadata, input, output, clean, verbose)
    
    lines <- readUTF8(output)
    
    ## replace footnotes with sidenotes
    lines = process_footnotes(lines)
    
    ## set captions as sidenotes
    lines = process_captions(lines)
    
    writeUTF8(lines, output)
    output
  }
  
  config
}

process_headers = function(lines) {
  template <- '<h%s>%s</h%s>'
  pattern <- sprintf(template, '([1-5])', '(.*)', '\\1')
  
  headers <- grep(pattern, lines, value=TRUE)
  levels <- as.integer(gsub(pattern, '\\1', headers))
  
  matches <- regexpr(pattern, lines)
  
  f <- function(header, level) {
    sub(pattern, sprintf(template, level, '\\2', level), header)
  }
  
  regmatches(lines, matches) <- mapply(f, headers, levels+1L, USE.NAMES = FALSE)
  
  lines
}

process_captions = function(lines) {
  ## convert table captions enlosed in <caption> tags into <p class="caption">
  pattern <- '(^<caption>)(.*)(</caption>$)'
  idx <- grep('^<table', lines)
  idx <- idx[grepl(pattern, lines[idx+1L])]
  captions <- lines[idx+1L]
  lines[idx+1L] <- lines[idx]
  lines[idx] <- gsub(pattern, '<p class="caption">\\2</p>', captions)
  
  ## append 'sidenote' CSS class to figure and table captions
  lines = gsub('(?<=^<p class="caption)(?=">)', ' sidenote', lines, perl=TRUE)
  
  lines
}
