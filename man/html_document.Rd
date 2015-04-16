\name{html_document}

\alias{html_document}

\title{Use Bioconductor style to format R Markdown HTML output}

\description{
  This function sets the Bioconductor style in HTML documents rendered using R Markdown v2.
}

\usage{
html_document(css, theme = NULL, ...)
}

\arguments{

  \item{css}{character, additional CSS files to include}

  \item{theme}{Visual theme ("default", "cerulean", "journal", "flatly",
"readable", "spacelab", "united", or "cosmo"). Pass \code{NULL} for
no theme (in this case you can use the \code{css} parameter to add your own
styles).}

  \item{...}{Additional arguments, passed to \code{\link{html_document}}.}
  
}

\value{
R Markdown output format to pass to \code{rmarkdown::render}.
}

\author{
  Andrzej Oles, 2014
}

\seealso{
  \code{\link{pdf_document}}
}

\examples{
\dontrun{

# simple invocation
render("input.Rmd", BiocStyle::html_document())

# specify options
render("input.Rmd", BiocStyle::html_document(toc = TRUE))
}
}

\keyword{ manip }
