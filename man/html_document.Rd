\name{html_document}

\alias{html_document}

\title{Use Bioconductor style to format R Markdown HTML output}

\description{
Format for converting from R Markdown to an Bioconductor HTML document.
}

\usage{
html_document(toc = TRUE, number_sections = TRUE, fig_width = NA, fig_height = NA,
              fig_retina = NULL, self_contained = TRUE, css = NULL, 
              pandoc_args = NULL, ...)
}
\arguments{
\item{toc}{logical(1), \code{TRUE} to include a table of contents in the output}

\item{number_sections}{logical(1), \code{TRUE} to number section headings}

\item{fig_width}{numeric(1), default width (in inches) for figures}

\item{fig_height}{numeric(1), default width (in inches) for figures}

\item{fig_retina}{numeric(1), scaling to perform for retina displays}

\item{self_contained}{numeric(1), \code{TRUE} to produce a standalone HTML file with no external
dependencies, using data: URIs to incorporate the contents of linked
scripts, stylesheets, images, and videos. Note that even for self
contained documents MathJax is still loaded externally (this is
necessary because of it's size).}

\item{css}{character, one or more css files to include}

\item{pandoc_args}{character, additional command line options to pass to pandoc}

\item{\dots}{Additional arguments passed to \code{\link[rmarkdown]{html_document}}}
}

\value{
R Markdown output format to pass to \code{\link[rmarkdown]{render}}
}

\details{
\code{BiocStyle::html_document} format extends the \code{\link[rmarkdown]{html_document}} format.
See the \href{http://rmarkdown.rstudio.com/html_document_format.html}{online
documentation} and the package vignette "Authoring R Markdown Vignettes" for 
additional details on using the format,
}

\author{
  Andrzej Oleś <andrzej.oles@embl.de>, 2014-2017
}

\seealso{
  \code{\link[BiocStyle]{pdf_document}}, \code{\link[BiocStyle]{md_document}}
}

\examples{
\dontrun{

# simple invocation
render("input.Rmd", BiocStyle::html_document())

# specify options
render("input.Rmd", BiocStyle::html_document(toc = FALSE))
}
}

