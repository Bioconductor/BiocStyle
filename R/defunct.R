.defunct <- function (new,
                      package = NULL,
                      msg,
                      old = as.character(sys.call(sys.parent()))[1L]) {
  if (missing(msg)) {
    msg <- gettextf("'%s' is deprecated.\n", old)
    if (!missing(new)) 
      msg <- c(msg, gettextf("Use '%s' instead.\n", new))
    c(msg, if (!is.null(package)) gettextf("See help(\"Deprecated\") and help(\"%s-deprecated\").", 
                                           package) else gettext("See help(\"Deprecated\")"))
  }
  .Defunct(new, package, msg)
}

latex_old <- function(...) {
  .defunct("latex", old = "latex_old")
}

latex2 <- function(...) {
  .defunct("latex", old = "latex2")
}

pdf_document_old <- function(...) {
  .defunct("pdf_document", old = "pdf_document_old")
}

pdf_document2 <- function(...) {
  .defunct("pdf_document", old = "pdf_document2")
}

html_document_old <- function(...) {
  .defunct("html_document", old = "html_document_old")
}

html_document2 <- function(...) {
  .defunct("html_document", old = "html_document2")
}
