\name{markdown}

\alias{markdown}
\alias{Biocpkg}
\alias{Biocannopkg}
\alias{Biocexptpkg}
\alias{CRANpkg}

\title{Use Bioconductor style to format HTML vignettes}

\description{
  This function sets the Bioconductor style sheet to provide a
  consistent style across Bioconductor HTML vignettes.
}

\usage{
markdown(css.files, self.contained = TRUE)
}

\arguments{

  \item{css.files}{character vector containing the location of additional \code{.css} files.}

  \item{self.contained}{logical(1), should the content of the CSS \code{stylesheet} files be beincluded into the html file or should they be saved as separte files}
  
}

\details{
  Use is described in the \sQuote{Bioconductor CSS Style} vignette.
}

\value{
  No value is returned. The function is called for its side effect of setting the \code{markdown} and/or \code{knitr} specific options controlling the inclusion of the Bioconductor CSS style file in the HTML output.
}

\author{
  Andrzej Oles, 2014
}

\examples{
## location of the .css file
system.file(package = "BiocStyle", "css", "bioconductor.css")
}

\keyword{ manip }
