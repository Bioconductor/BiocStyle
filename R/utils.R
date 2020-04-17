modifyLines <- function (lines, from, to, replace = TRUE, before = FALSE, 
                         offset = 0L, fixed = TRUE, insert = "") {
  from <- grep(from, lines, fixed=fixed)[1L]
  
  # exit if nothing found
  if (is.na(from)) return(lines)
  
  if (missing(to)) {
    to <- from
  } else {
    to <- grep(to, lines, fixed=fixed)
    if (length(to)==0) return(lines) else to = to[to > from][1L]
  }
  
  offset <- rep_len(offset, 2L)
  from <- from + offset[1L]
  to <- to + offset[2L]
  
  if (isTRUE(replace)) { # substitute lines from-to
    c(lines[1:(from-1L)], insert, lines[(to+1L):length(lines)])
  } else {
    if (isTRUE(before)) # insert before
      c(lines[1:(from-1L)], insert, lines[from:length(lines)])
    else # insert after
      c(lines[1:to], insert, lines[(to+1L):length(lines)])
  }
}

readUTF8 = function(file, ...) readLines(file, encoding = 'UTF-8', warn = FALSE, ...)

writeUTF8 = function(text, ...) writeLines(enc2utf8(text), ..., useBytes = TRUE)

biocTempfile = function(name) {
  biocDir = file.path(tempdir(), "BiocStyle")
  if (!dir.exists(biocDir))
    dir.create(biocDir)
  
  file.path(biocDir, name)
}

sub_ext = function(file, ext) sub("([.][[:alnum:]]+)?$", ext, file)

## recursively merge two lists
merge_lists <- function (base_list, overlay_list) {
  if (length(base_list) == 0L)
    overlay_list
  else if (length(overlay_list) == 0L)
    base_list
  else {
    merged_list <- base_list
    for (name in names(overlay_list)) {
      base <- base_list[[name]]
      overlay <- overlay_list[[name]]
      if (is.list(base) && is.list(overlay))
        merged_list[[name]] <- merge_lists(base, overlay)
      else 
        merged_list[[name]] <- overlay_list[[name]]
    }
    merged_list
  }
}

## print file content
.print.file <- function(file, scoped = FALSE) {
  type = unlist(strsplit(file, split=".", fixed=TRUE))
  type = tolower(type[length(type)])
  
  paste(c(
    switch(type, 
           js  = '<script type="text/javascript">',
           css = if (isTRUE(scoped)) '<style type="text/css" scoped>'
           else '<style type="text/css">',
           NULL),
    ## actual file content
    readLines(file),
    switch(type, 
           js  = '</script>\n',
           css = '</style>\n',
           NULL)
  ), collapse = '\n')
}

loadBioconductorStyleFile <- function(file, opts=NULL) {
  sprintf("\\RequirePackage[%s]{%s}",
          paste(opts, collapse = ","),
          sub(".sty$", "", file))
}

copyResource <- function(file, dir) {
  filename <- basename(file)
  file.copy(file, file.path(dir, filename))
  setNames(filename, names(file))
}

