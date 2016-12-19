\name{latex2}

\alias{latex2}

\title{Use Bioconductor styles to format LaTeX vignettes}
\description{

  This function inserts code into a document preamble to provide a
  consistent style to Bioconductor vignettes.

}
\usage{
latex2(..., width, titlecaps = TRUE, short.fignames=FALSE, fig.path,
       use.unsrturl=TRUE, relative.path=FALSE)
}
\arguments{

  \item{\dots}{Additional arguments, passed to \code{\link{options}}.}

  \item{width}{integer(1), maximum number of columns on a line used in
    printing. See \code{\link{options}}.}
    
  \item{titlecaps}{logical(1), emphasize the first sentence of float captions}

  \item{short.fignames}{logical(1), indicates whether \code{\\incfig}
    figures should be inserted and referred to using short names equal to
    corresponding code chunk labels without any additional prefix.}

  \item{fig.path}{character(1), custom prefix to be used for figure file
    names when used with \code{knitr}; has no effect when compiled with
    \code{Sweave}. For details see \code{\link[knitr]{opts_chunk}}.}

  \item{use.unsrturl}{logical(1), indicating that the \sQuote{unsrturl}
    style will be used (\code{\\bibliographystyle} command \emph{not}
    required).}
  
  \item{relative.path}{logical(1), copy supporting LaTeX files and use relative
    paths rather than absolute paths to system files.}
}
\details{
  Use is described in the \sQuote{Bioconductor LaTeX Style 2.0} vignette.
  
  By default the 'unsrturl' bibliography style is used, which
  automatically creates links to URL, DOIs, etc. Use a different
  \code{bibliographystyle} with \code{use.unsrturl=FALSE} and standard
  LaTeX commands for styling bibliographies.
}

\value{

  The return value is a character string directing LaTex to use the
  Bioconductor style file.

  A side effect is setting any options specified as arguments.

}
\author{
   Andrzej Oleś, Martin Morgan, Wolfgang Huber
}

\examples{
## location of the .sty file
BiocStyle:::bioconductor2.sty
}

\keyword{ manip }
