md_document <- function(toc = TRUE, ...) {
  
  ## load the package to expose macros
  require(BiocStyle, quietly = TRUE)
  
  base_format = rmarkdown::md_document(toc = toc, md_extensions = "-markdown_in_html_blocks", ...)
  
  ## append kramdown toc label at the very beginning of document body
  if ( isTRUE(toc) ) {
    base_format$pre_processor <- function(metadata, input_file, runtime, knit_meta, files_dir, output_dir) {
      lines <- readUTF8(input_file)
      
      delimiters <- grep("^---\\s*$", lines)
      
      if ( length(delimiters) >= 2L ) {
        head <- lines[(delimiters[1L]):(delimiters[2L])]
        body <- lines[(delimiters[2L]+1L):length(lines)]
      }
      else {
        head <- NULL
        body <- lines
      }
      
      lines <- c(head, "{:toc}", body)
      
      writeUTF8(lines, input_file)
    }
  }
  
  post_processor <- function(metadata, input_file, output_file, clean, verbose) {
    lines <- readUTF8(output_file)
    
    # move all headers one level down (for proper formatting when embedded in the website)
    pattern <- '^(#+ )'
    lines <- gsub(pattern, '#\\1', lines)
    
    # make sure there is a newline before headers (pandoc removes them if preceeded by, e.g., <p>...</p>)
    idx <- grep(pattern, lines)
    idx <- idx[lines[idx - 1L] != ""]
    if ((l = length(idx))) {
      idx <- idx + 0:(l-1L)
      res <- vector(mode="character", length(lines)+l)
      res[-idx] <- lines
      lines <- res
    }
    
    # remove empty line between TOC and kramdown reference name
    lines <- modifyLines(lines, from="{:toc}", offset=-1L, insert=NULL)
    
    header <- NULL
    
    # add document title
    if ( !is.null(metadata$title) )
      header <- c(sprintf("# %s", metadata$title), "{:.no_toc}", "", header)
    
    # add author affiliations
    if ( !is.null((auth_affil <- auth_affil_html(metadata))) )
      header <- c(header, auth_affil, "")
    
    # TOC header
    header <- c(header, "## Contents", "{:.no_toc}", "")
    
    lines <- c(header, lines)
    
    writeUTF8(lines, output_file)
    output_file
  }
  
  # return format
  rmarkdown::output_format(knitr = NULL,
                           pandoc = list(args = "--atx-headers"),
                           clean_supporting = FALSE,
                           post_processor = post_processor,
                           base_format = base_format)
}
