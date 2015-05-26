\name{latex}
\alias{latex}
\title{Use Bioconductor styles to format LaTeX vignettes}
\description{

  This function inserts code into a document preamble to provide a
  consistent style to Bioconductor vignettes

}
\usage{
latex(..., width=90, short.fignames=FALSE, fig.path, error=FALSE,
      use.unsrturl=TRUE)
}
\arguments{

  \item{\dots}{Additional arguments, passed to \code{\link{options}}.}

  \item{width}{integer(1), maximum number of columns on a line used in
    printing. See \code{\link{options}}.}

  \item{short.fignames}{logical(1), indicates whether \code{\\incfig}
    figures should be inserted and referred to using short names equal to
    corresponding code chunk labels without any additional prefix.}

  \item{fig.path}{character(1), custom prefix to be used for figure file
    names when used with \code{knitr}; has no effect when compiled with
    \code{Sweave}. For details see \code{\link[knitr]{opts_chunk}}.}

  \item{error}{logical(1), behavior of \code{knitr} when errors occur in
    vignette code chunks, has no effect when compiled with
    \code{Sweave}. The default (\code{FALSE}) means that errors stop
    vignette processing, which is appropriate when packages are being
    built and checked. For details see \code{\link[knitr]{opts_chunk}}.}
    
  \item{use.unsrturl}{logical(1), indicating that the \sQuote{unsrturl}
    style will be used (\code{\\bibliographystyle} command \emph{not}
    required).}

}
\details{
  Use is described in the \sQuote{Bioconductor LaTeX Style} vignette.
  
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
  Martin Morgan \url{mtmorgan@fhcrc.org}, Andrzej Oles, Wolfgang Huber
}

\examples{
## location of the .sty file
BiocStyle:::bioconductor.sty
}

\keyword{ manip }
