\name{md_document}

\alias{md_document}

\title{Enable BiocStyle macros and a hyperlinked TOC in markdown output}

\description{
Format for converting from R Markdown to an Bioconductor HTML document.
}

\usage{
md_document(toc = TRUE, toc_depth = 3, ...)
}
\arguments{
\item{toc}{\code{TRUE} to include a table of contents in the output}

\item{toc_depth}{Depth of headers to include in table of contents}

\item{...}{Additional function arguments to pass to the base R Markdown \code{\link[rmarkdown]{md_document}} 
  output formatter}
}

\value{
R Markdown output format to pass to \code{\link[rmarkdown]{render}}
}

\details{
The \code{BiocStyle::md_document} format extends the \code{\link[rmarkdown]{md_document}} format by a hyperlinked TOC. It also loads the  BiocStyle package namespace to enable the use of helper functions, such as \code{\link{Biocpkg}}.
}

\author{
  Andrzej Oleś <andrzej.oles@embl.de>, 2015
}

\seealso{
  \code{\link[BiocStyle]{html_document}}, \code{\link[BiocStyle]{pdf_document}}
}

\examples{
\dontrun{

# simple invocation
render("input.Rmd", BiocStyle::md_document())

}
}

