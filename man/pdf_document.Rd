\name{pdf_document}

\alias{pdf_document}

\title{Use Bioconductor style to format R Markdown PDF output}

\description{
  This function sets the Bioconductor style in PDF documents rendered using R Markdown v2.
}

\usage{
pdf_document(toc = TRUE, number_sections = TRUE, use.unsrturl = TRUE, includes, ...)
}

\arguments{

  \item{toc}{logical(1), \code{TRUE} to include a table of contents in the output.}

  \item{number_sections}{logical(1), \code{TRUE} to number section headings.}
    
  \item{use.unsrturl}{logical(1), indicating that the \sQuote{unsrturl}
    style will be used (\code{\\bibliographystyle} command \emph{not}
    required).}
    
  \item{includes}{Named list of additional content to include within the document}

  \item{...}{Additional arguments, passed to \code{\link{pdf_document}}.}
  
}

\value{
R Markdown output format to pass to \code{rmarkdown::render}.
}

\author{
  Andrzej Oleś <andrzej.oles@embl.de>, 2014-2015
}

\seealso{
  \code{\link{html_document}}, \code{\link[BiocStyle]{md_document}}
}

\examples{
\dontrun{

# simple invocation
render("input.Rmd", BiocStyle::pdf_document())

# specify an option for latex engine
render("input.Rmd", BiocStyle::pdf_document(toc = FALSE))
}
}

\keyword{ manip }
