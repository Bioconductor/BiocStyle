\name{latex}
\alias{latex}
\title{Use Bioconductor styles to format LaTeX vignettes}
\description{

  This function inserts code into a document preamble to provide a
  consistent style to Bioconductor vignettes

}
\usage{
latex(..., width=90)
}
\arguments{

  \item{\dots}{Additional arguments, passed to \code{\link{options}}.}

  \item{width}{integer(1) Maximum number of columns on a line used in
    printing. See \code{\link{options}}.}

}
\details{
  Use is described in the \sQuote{Bioconductor LaTeX Style} vignette.
}
\value{

  The return value is a character string directing LaTex to use the
  Bioconductor style file.

  A side effect is setting any options specified as arguments.

}
\author{
  Martin Mogan \url{mtmorgan@fhcrc.org}, Wolfgang Huber
}

\examples{
## location of the .sty file
system.file(package = "BiocStyle", "sty", "Bioconductor.sty")
}

\keyword{ manip }
