\name{latex}
\alias{latex}
\title{Use Bioconductor styles to format LaTeX vignettes}
\description{

  This function inserts code into a document preamble to provide a
  consistent style to Bioconductor vignettes

}
\usage{
latex(...)
}
\arguments{
  \item{\dots}{Additional arguments, currently not used.}
}
\details{
  Use is described in the \sQuote{Bioconductor LaTeX Style} vignette.
}
\value{
  A character string directing LaTex to use the Bioconductor style file.
}
\author{
  Martin Mogan \url{mtmorgan@fhcrc.org}, Wolfgang Huber
}

\examples{
## location of the .sty file
system.file(package = "BiocStyle", "sty", "Bioconductor.sty")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
