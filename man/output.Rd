\name{output}

\alias{output}

\title{Output format of an R Markdown document}

\description{
  Helper function to determine the document's current pandoc output format.
}

\usage{
output()
}

\details{
The function is useful for defining different behavior depending on the output
format, e.g. figure settings.
}

\value{
  A character string specifying the pandoc output format.
}

\author{
  Andrzej Oleś <andrzej.oles@embl.de>, 2016
}

\examples{
\dontrun{
## Switch between SVG and PDF figures depending on document output format
knitr::opts_chunk$set(
  dev = switch(output(), html = "svg", latex = "pdf")
  )
}
}
