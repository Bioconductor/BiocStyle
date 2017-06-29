html_fragment <- function(...,
                          toc = TRUE,
                          number_sections = TRUE,
                          theme = NULL,
                          highlight = NULL) {
  
  ## enable BiocStyle macros
  require(BiocStyle, quietly = TRUE)
  
  # call rmarkdown::html_document with "default" template and substitute it only
  # afterwards in order to retain original mathjax functionality
  output_format_base <- rmarkdown::html_document(...,
                           toc = toc,
                           number_sections = number_sections,
                           theme = theme,
                           highlight = highlight)

  template = system.file("resources", "fragment.html", package="BiocStyle")
  idx = which(output_format_base$pandoc$args=="--template") + 1L
  output_format_base$pandoc$args[idx] <- template
  
  base_format <- function(number_sections, output_format_base) {
    
    post_processor <- function(metadata, input_file, output_file, clean, verbose) {
      lines <- readUTF8(output_file)
      
      ## move all headers one level down (for proper formatting when embedded in the website)
      template <- '<h%s>%s</h%s>'
      pattern <- sprintf(template, '([1-5])', '(.*)', '\\1')
      
      headers <- grep(pattern, lines, value=TRUE)
      levels <- as.integer(gsub(pattern, '\\1', headers))
      
      matches <- regexpr(pattern, lines)
      
      f <- function(header, level) {
        sub(pattern, sprintf(template, level, '\\2', level), header)
      }
      
      regmatches(lines, matches) <- mapply(f, headers, levels+1L, USE.NAMES = FALSE)
      
      ## add author affiliations
      lines <- modifyLines(lines, from='<!-- AUTH AFFIL -->', insert=auth_affil_html(metadata))
      
      ## replace footnotes with sidenotes
      lines = process_footnotes(lines)
      
      writeUTF8(lines, output_file)
      output_file
    }
    
    rmarkdown::output_format(
      knitr = NULL,
      pandoc = NULL,
      post_processor = post_processor,
      base_format = output_format_base)
  }
  
  bookdown::html_document2(
    output_format_base = output_format_base,
    number_sections = number_sections,
    base_format = base_format
  )
}
