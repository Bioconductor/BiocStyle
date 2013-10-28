\name{latex}
\alias{latex}
\title{Use Bioconductor styles to format LaTeX vignettes}
\description{

  This function inserts code into a document preamble to provide a
  consistent style to Bioconductor vignettes

}
\usage{
latex(..., width=90, short.fignames=FALSE, fig.path, bibstyle="unsrturl")
}
\arguments{

  \item{\dots}{Additional arguments, passed to \code{\link{options}}.}

  \item{width}{integer(1), maximum number of columns on a line used in
             printing. See \code{\link{options}}.}

  \item{short.fignames}{logical, indicates whether \code{\\incfic}
    figures should be inserted and refered to using short names equal to
    corresponding code chunk labels without any additional prefix.}

  \item{fig.path}{character, custom prefix to be used for figure file
    names when used with \code{knitr}; has no effect when compiled with
    \code{Sweave}. For details see \code{\link[knitr]{opts_chunk}}.}
    
  \item{bibstyle}{character, bibliography style to be used, as provided to the LaTeX \code{\\bibliographystyle} command.}

}
\details{
  Use is described in the \sQuote{Bioconductor LaTeX Style} vignette.
  
  The \code{bibstyle} option is used to customize the bibliography appearance. It's value is passed to the LaTeX \code{\\bibliographystyle} command. By default the 'unsrturl' is used, which automatically creates links to URL, DOIs, etc. To suppress setting \code{bibliographystyle} use \code{bibstyle=NA}.
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
