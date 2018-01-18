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


#' Defunct functions in package \sQuote{BiocStyle}
#'
#' These functions are defunct and no longer available.
#'
#' The following functions are no longer available; use the replacement
#' indicated below:
#' \itemize{
#'   \item latex_old, latex2: \code{\link{latex}}
#'   \item pdf_document_old, pdf_document2: \code{\link{pdf_document}}
#'   \item html_document_old, html_document2: \code{\link{html_document}}
#' }
#'
#' @name BiocStyle-defunct
#' @aliases latex_old latex2 pdf_document_old pdf_document2 html_document_old
#'   html_document2
NULL


#' @export
latex_old <- function(...) {
  .defunct("latex", old = "latex_old")
}

#' @export
latex2 <- function(...) {
  .defunct("latex", old = "latex2")
}

#' @export
pdf_document_old <- function(...) {
  .defunct("pdf_document", old = "pdf_document_old")
}

#' @export
pdf_document2 <- function(...) {
  .defunct("pdf_document", old = "pdf_document2")
}

#' @export
html_document_old <- function(...) {
  .defunct("html_document", old = "html_document_old")
}

#' @export
html_document2 <- function(...) {
  .defunct("html_document", old = "html_document2")
}
