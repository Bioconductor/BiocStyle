modifyLines <- function (..., lines, from, to, replace = TRUE, before = FALSE, offset = 0L) {
  from <- grep(from, lines, fixed=TRUE)[1L]
  
  # exit if nothing found
  if (is.na(from)) return(lines)
  
  if (missing(to)) {
    to <- from
  } else {
    to <- grep(to, lines, fixed=TRUE)
    if (length(to)==0) return(lines) else to = to[to > from][1L]
  }
  
  offset <- rep_len(offset, 2L)
  from <- from + offset[1L]
  to <- to + offset[2L]
  
  if (isTRUE(replace)) { # substitute lines from-to
    c(lines[1:(from-1L)], ..., lines[(to+1L):length(lines)])
  } else {
    if (isTRUE(before)) # insert before
      c(lines[1:(from-1L)], ..., lines[from:length(lines)])
    else # insert after
      c(lines[1:from], ..., lines[(from+1L):length(lines)])
  }
}
