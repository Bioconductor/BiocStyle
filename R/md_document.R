md_document <- function(toc = TRUE, ...) {
  
  ## load the package to expose macros
  require(BiocStyle, quietly = TRUE)
  
  base_format = rmarkdown::md_document(toc = toc, md_extensions = "-markdown_in_html_blocks", ...)
  
  ## append kramdown toc label at the very beginning of document body
  if ( isTRUE(toc) ) {
    base_format$pre_processor <- function(metadata, input_file, runtime, knit_meta, files_dir, output_dir) {
      lines <- readUTF8(input_file)
      
      doc <- rmarkdown:::partition_yaml_front_matter(lines)
      lines <- c(doc$front_matter, "{:toc}", doc$body)
      
      writeUTF8(lines, input_file)
    }
  }
  
  post_processor <- function(metadata, input_file, output_file, clean, verbose) {
    lines <- readUTF8(output_file)
    
    # convert absolute paths to relative, see https://github.com/rstudio/rmarkdown/issues/587
    lines = gsub(file.path(dirname(output_file), ''), '', lines, fixed=TRUE)
    
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
  
  knitr = knitr_options(opts_knit = list(width = 80L))
  
  # return format
  rmarkdown::output_format(knitr = knitr,
                           pandoc = list(args = "--atx-headers"),
                           clean_supporting = FALSE,
                           post_processor = post_processor,
                           base_format = base_format)
}
