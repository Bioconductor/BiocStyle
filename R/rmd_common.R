## r markdown pre-processor function modifying document front matter
pre_processor = function(metadata, input_file, runtime, knit_meta, files_dir, output_dir) {
  entries = c()
  
  ## the metadata may contain r code, so we extract it from the knitted 
  ## document instead of using the 'metadata' argument
  lines <- readUTF8(input_file)
  lines <- rmarkdown:::partition_yaml_front_matter(lines)
  front_matter <- lines$front_matter
  
  ## process the package field
  idx = grep("^package:", front_matter)
  if ( length(idx)==1L ) {
    ## use yaml parser to clean-up input from possible quotes etc.
    yaml_list <- yaml.load(front_matter[idx])
    package <- trimws(yaml_list$package)
    ## append version number if the field contains valid package name 
    if ( isTRUE(grepl("^[[:alnum:].]+[[:alnum:]]+$", package)) )
      front_matter[idx] <- sprintf("package: %s", pkg_ver(package))
  }
  
  ## add date if missing
  if ( is.null(metadata$date) )
    entries <- c(entries, sprintf("date: %s", doc_date()))
  
  ## append new metadata entries to yaml front matter
  l = length(front_matter)
  front_matter <- c(front_matter[-l],
                    entries,
                    front_matter[l])
  
  writeUTF8(c(front_matter, lines$body), input_file)
  
  NULL
}
