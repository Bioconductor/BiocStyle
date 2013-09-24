\name{latex}
\alias{latex}
\title{Use Bioconductor styles to format LaTeX vignettes}
\description{

  This function inserts code into a document preamble to provide a
  consistent style to Bioconductor vignettes

}
\usage{
latex(..., width=90, short.fignames=FALSE, fig.path)
}
\arguments{

  \item{\dots}{Additional arguments, passed to \code{\link{options}}.}

  \item{width}{integer(1), maximum number of columns on a line used in
             printing. See \code{\link{options}}.}

  \item{short.fignames}{logical, indicates whether \code{\\incfic} figures should be inserted and refered to using short names equal to corresponding code chunk labels without any additional prefix.}

  \item{fig.path}{character, custom prefix to be used for figure file names when used with \code{knitr}; has no effect when compiled with \code{Sweave}. For details see \code{\link[knitr]{opts_chunk}}.}

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
  Martin Morgan \url{mtmorgan@fhcrc.org}, Andrzej Oles, Wolfgang Huber
}

\examples{
## location of the .sty file
system.file(package = "BiocStyle", "sty", "Bioconductor.sty")
}

\keyword{ manip }
