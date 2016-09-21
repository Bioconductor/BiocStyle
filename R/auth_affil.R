parse_auth_affil = function(m) {
  auth <- m$author
  
  ## missing author metadata
  if ( is.null(auth) )
    return()
  
  res <- list(
    names = NULL,
    affil = NULL,
    affil_marks = NULL,
    email = NULL,
    email_marks = NULL
  )
  
  if (is.list(auth)) {
    ## author list with additional metadata
    if (is.null(names(auth))) {
      ## multiple authors
      names <- sapply(auth, .subset2, "name")
      email <- sapply(auth, .subset2, "email")
      affil <- sapply(auth, .subset2, "affiliation")
      
      ## author names
      res$names <- names
      
      ## assign IDs to unique affiliations
      affil_uniq <- unique(unlist(affil))
      
      if ( !is.null(affil_uniq) ) {
        affil_ids <- setNames(seq_along(affil_uniq), affil_uniq)
        affil_marks <- sapply(affil, function(a) paste(sort(affil_ids[a]), collapse = ',') )
        
        res$affil <- affil_uniq
        res$affil_marks <- affil_marks
      }
      
      ## assign IDs to emails
      email_uniq <- unique(unlist(email))
      
      if ( !is.null(email_uniq) ) {
        email_ids <- setNames(sapply(seq_along(email_uniq), function(i) paste(rep("*", i), collapse="")), email_uniq)
        email_marks <- sapply(email, function(e) paste(sort(email_ids[e]), collapse = ','))
        
        res$email <- email_uniq
        res$email_marks <- email_marks
      }
    }
    else {
      ## single author
      res$names <- auth$name
      res$email <- auth$email
      res$affil <- auth$affiliation
    }
  }
  ## plain author field
  else {
    res$names = auth
  }
  
  res
}

auth_affil_html = function(m) {
  m <- parse_auth_affil(m)
  
  ## missing author metadata
  if ( is.null(m) )
    return()
  
  ## construct author list
  authors <-
  if ( is.null(m$affil_marks) && is.null(m$email_marks) ) {
    m$names
  } else {
    marks <- paste0(m$affil_marks, m$email_marks)
    idx <- marks != ""
    marks[idx] <- sprintf('<span class="affil-mark">%s</span>', marks[idx])
    paste0(m$names, marks)
  }

  if ((l = length(authors)) > 2)
    authors <- c(paste(authors[1:(l-1L)], collapse=", "), authors[l])
 
  authors <- paste(authors, collapse=" and ")
 
  res <- sprintf('<p class="author-name">%s</p>', authors)
  
  ## construct affiliations list
  if ( !is.null(m$affil) ) {
    if ( !is.null(m$affil_marks) ) {
      affiliations <- sprintf('<span class="affil-mark">%s</span>%s', seq_along(m$affil), m$affil)
    } else {
      affiliations <- m$affil
    }
    affiliations <- paste(affiliations, collapse="<br>")
    res <- c(res, sprintf('<p class="author-affiliation">%s</p>', affiliations))
  }
  
  ## construct email list
  if ( !is.null(m$email) ) {
    emails <- sprintf('<a href="mailto:%s">%s</a>', m$email, m$email)
    if ( !is.null(m$email_marks) ) {
      emails <- sprintf('<span class="affil-mark">%s</span>%s', m$email_marks[m$email_marks!=""], emails)
      emails <- paste(emails, collapse="<br>")
    }
    res <- c(res, sprintf('<p class="author-email">%s</p>', emails))
  }
  
  res
}
