html_fragment <- function(...,
                          toc = TRUE,
                          number_sections = TRUE,
                          pandoc_args = NULL) {
  
  ## enable BiocStyle macros
  require(BiocStyle, quietly = TRUE)
  
  output_format_base <- rmarkdown::html_fragment(...,
                           toc = toc,
                           number_sections = number_sections,
                           pandoc_args = pandoc_args)
  
  base_format <- function(number_sections, output_format_base) {
    
    post_processor <- function(metadata, input_file, output_file, clean, verbose) {
      lines <- readUTF8(output_file)
      
      # move all headers one level down (for proper formatting when embedded in the website)
      template <- '<h%s>%s</h%s>'
      pattern <- sprintf(template, '([1-5])', '(.*)', '\\1')
      
      headers <- grep(pattern, lines, value=TRUE)
      levels <- as.integer(gsub(pattern, '\\1', headers))
      
      matches <- regexpr(pattern, lines)
      
      f <- function(header, level) {
        sub(pattern, sprintf(template, level, '\\2', level), header)
      }
      
      regmatches(lines, matches) <- mapply(f, headers, levels+1L, USE.NAMES = FALSE)
      
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
