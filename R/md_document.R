md_document <- function(toc = TRUE, toc_depth = 3, ...) {
  
  ## load the package to expose macros
  require(BiocStyle, quietly = TRUE)
  
  base_format = rmarkdown::md_document(toc = FALSE, md_extensions = "-markdown_in_html_blocks", ...)
  
  if (isTRUE(toc)) {
    generate_toc <- function(input, output, template, toc_depth, verbose = FALSE) {
      quoted <- rmarkdown:::quoted
      pandoc <- rmarkdown:::pandoc
      
      # input file, toc template and toc args
      args <- c(input, "--template", template, "--table-of-contents", "--toc-depth", toc_depth)
      
      # set pandoc stack size
      stack_size <- getOption("pandoc.stack.size", default = "512m")
      args <- c(c("+RTS", paste0("-K", stack_size), "-RTS"), args)
      
      # build the conversion command
      cmd <- paste(quoted(pandoc()), paste(quoted(args), collapse = " "), "--to html --from markdown",
              "|", quoted(pandoc()), "--to markdown --from html --output", quoted(output), collapse = " ")
      
      # show it in verbose mode
      if (verbose) cat(cmd, "\n")
      
      # run the conversion
      rmarkdown:::with_pandoc_safe_environment({
        result <- system(cmd)
      })
      if (result != 0)
        stop("pandoc document conversion failed with error ", result, call. = FALSE)
      
      invisible()
    }
    
    base_format$pre_processor <- function(metadata, input_file, runtime, knit_meta, files_dir, output_dir) {
      ## set up file names and paths
      toc_file <- sub("([^.]+\\.)utf8\\.md$", "\\1toc.md", input_file)
      toc_template = file.path(resources, "md", "toc.template")
      
      ## generate the TOC file
      generate_toc(input_file, toc_file, toc_template, toc_depth)
      
      ## inject the temporary TOC file into rmarkdown intermediates
      intermediates = get("intermediates", envir = parent.frame())
      assign("intermediates", c(intermediates, toc_file), envir = parent.frame())
      
      ## return the additional argument to pandoc for TOC inclusion
      c("--include-before-body", toc_file)
    }
  }
  
  post_processor <- function(metadata, input_file, output_file, clean, verbose) {
    lines <- readUTF8(output_file)
    
    # move all headers one level down (for proper formatting when embedded in the website)
    pattern <- '^(#+ )'
    lines <- gsub(pattern, '#\\1', lines)
    
    # make sure there is a newline before headers (pandoc removes them if preceeded by, e.g., <p>...</p>)
    idx <- grep(pattern, lines)
    idx <- idx[lines[idx - 1] != ""]
    idx <- idx + 0:(length(idx)-1)
    res <- vector(mode="character", length(lines)+length(idx))
    res[-idx] <- lines
    
    writeUTF8(res, output_file)
    output_file
  }
  
  # return format
  rmarkdown::output_format(knitr = NULL,
                           pandoc = list(args = "--atx-headers"),
                           post_processor = post_processor,
                           base_format = base_format)
}
  
