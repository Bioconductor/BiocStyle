\name{helpers}

\alias{doc_date}
\alias{pkg_ver}

\title{Specify Rmarkdown document metadata}

\description{
  Helper functions for including metadata in the document header.
}

\usage{
doc_date()
pkg_ver(pkg)
}

\arguments{
  \item{pkg}{character(1), package name}
}

\details{
Use \code{doc_date} to include document compilation date in the document metadata field 'date', and \code{pkg_ver} for package version specification in the 'package' field.
}

\value{
  Markdown-formatted character string.
}

\author{
  Andrzej Oleś <andrzej.oles@embl.de>, 2014-2015
}

\examples{

## current date
doc_date()

## package name with version
pkg_ver("BiocStyle")

}

\keyword{ manip }
