html_fragment <- function(...,
                          toc = TRUE,
                          number_sections = TRUE,
                          fig_width = 5.67,
                          fig_height = fig_width,
                          fig_align = "right",
                          fig_crop = TRUE,
                          theme = NULL,
                          highlight = NULL,
                          navlinks = TRUE,
                          navlinks_top = TRUE,
                          navlinks_level = 3) {
  
  ## enable BiocStyle macros
  require(BiocStyle, quietly = TRUE)
  
  knitr <- knitr_options(opts_knit = list(width = 80L),
                         # remove figure margins
                         opts_chunk = list(crop = isTRUE(fig_crop),
                                           fig.align = fig_align),
                         knit_hooks = list(crop = hook_pdfcrop))
  
  post_processor <- function(metadata, input, output, clean, verbose) {
    lines <- readUTF8(output)
    
    ## add author affiliations
    lines <- modifyLines(lines, from='<!-- AUTH AFFIL -->', insert=auth_affil_html(metadata))
    
    ## append "References" section header
    if ( !is.null(metadata$bibliography) )
      lines <- add_refs_header(lines)
    
    ## move all headers one level down (for proper formatting when embedded in the website)
    lines <- process_headers(lines)
    
    writeUTF8(lines, output)
    output
  }
  
  config <- rmarkdown::output_format(
    knitr = knitr,
    pandoc = NULL,
    post_processor = post_processor,
    base_format = bookdown::html_document2(
      toc = toc,
      number_sections = number_sections,
      fig_width = fig_width,
      fig_height = fig_height,
      theme = theme,
      highlight = highlight,
      ...
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
    
    ## add navigation
    if (isTRUE(navlinks))
      lines <- add_navigation(lines, navlinks_top, navlinks_level)
    
    writeUTF8(lines, output)
    output
  }
  
  config
}

add_refs_header = function(lines) {
  refs <- grep('^<div id="refs" class="references">$', lines)
  
  if ( !isTRUE(grepl('<h1>.*</h1>', lines[refs-1L])) )
    lines <- c(lines[1:refs],
               '<h1>References</h1>',
               lines[(refs+1L):length(lines)])
  
  lines
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

add_navigation = function(lines, top, level) {
  ## match to all section div's
  pattern <- sprintf('^<div id="(.*)" class="section level([1-%d]).*">$', level)
  idx <- grep(pattern, lines)
  sections <- lines[idx]
  sections_length <- length(sections)
  
  ## extract section metadata
  section_ids <- sub(pattern, '#\\1', sections)
  section_levels <- as.integer(sub(pattern, '\\2', sections))
  section_names <- sub('<h([1-6])>(?:<span class="header-section-number">[0-9.]*</span> )?(.*)</h\\1>', '\\2', lines[idx+1L])
  
  seq_indices = seq_along(sections)
  
  ## add virtual top section at level 0
  if (isTRUE(top)) {
    section_ids <- c("#top", section_ids)
    section_levels <- c(0L, section_levels)
    section_names <- c("Top", section_names)
    seq_indices <- seq_indices + 1L
  }
  
  ## index of previous section on the same level
  section_prev <- sapply(seq_indices, function(i) {
    level = section_levels[i]
    neighbor <- which(section_levels==level)
    level_up <- which(section_levels==level-1L)
    level_up <- max(level_up[level_up<i], 0L)
    neighbor <- neighbor[neighbor<i & neighbor>level_up]
    neighbor[length(neighbor)][1L]
  })
  
  ## index of next section on the same level
  section_next <- sapply(seq_indices, function(i) {
    level = section_levels[i]
    neighbor <- which(section_levels==level)
    level_up <- which(section_levels==level-1L)
    level_up <- min(level_up[level_up>i], sections_length+1L)
    neighbor[neighbor>i & neighbor<level_up][1L]
  })
    
  ## index of parent section
  section_up <- sapply(seq_indices, function(i) {
    level = section_levels[i]
    level_up <- which(section_levels[1:i]==level-1L)
    level_up[length(level_up)][1L]
  })
  
  ## join navigation for sections and their immediate subsections
  merged <- c(FALSE, idx[-length(idx)]+2L == idx[-1L])
  merged_ids <- which(merged) + isTRUE(top)
  section_ids[merged_ids] <- section_ids[merged_ids - 1L]
  idx <- idx[!merged]
  
  ## move links to next subsections from fused sections up to their parents
  section_down <- rep(NA_integer_, sections_length)
  section_down[c(merged[-1L], FALSE)] <- section_next[merged]
  
  ## create links
  create_link <- function(v=0L, id=section_ids[v], name=section_names[v], icon)
    ifelse(is.na(v), "", sprintf('<span class="nav-icon">%s</span> <a href="%s">%s</a><br/>', icon, id, name))
  
  links <- paste0(
    create_link(id="#TOC", name="Table of Contents", icon="&#8803;"),
    create_link(section_next, icon="&#9656;"),
    create_link(section_prev, icon="&#9666;"),
    create_link(section_up, icon="&#9652;"),
    create_link(section_down, icon="&#9662;")
  )[!merged]
  
  ## preallocate the results vector and populate it with original lines
  idx_length <- length(idx)
  res <- vector(mode = "character", length = length(lines)+idx_length)
  idx <- idx + seq_len(idx_length)
  res[-idx] <- lines
  
  ## insert links
  res[idx] <- sprintf('<p class="sidenote">%s</p>', links)
  
  res
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
