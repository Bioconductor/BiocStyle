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

opts = function() knitr:::new_defaults()
