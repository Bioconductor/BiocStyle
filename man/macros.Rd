\name{macros}

\alias{Biocpkg}
\alias{Biocannopkg}
\alias{Biocexptpkg}
\alias{CRANpkg}
\alias{Rpackage}
\alias{Githubpkg}

\title{Link to packages on Bioconductor, CRAN and GitHub}

\description{
  Functions for adding links to Bioconductor, CRAN and GitHub packages into R Markdown documents.
}

\usage{
Biocpkg(pkg)
CRANpkg(pkg)
Githubpkg(repo, pkg)
Rpackage(pkg)
}

\arguments{
  \item{pkg}{character(1), package name}
  \item{repo}{Repository address in the format username/repo[/subdir]}
}

\details{
Use \code{Biocpkg} for Bioconductor software, annotation and experiment data packages. The function automatically includes a
link to the release landing page or if the package is only in devel, to the devel landing page.

Use \code{CRANpkg} for R packages available on CRAN. The function automatically includes a link to the FHCRC CRAN mirror landing page.

Use \code{Githubpkg} for R packages available on GitHub. The \code{repo} should include the repository address in the format username/repo[/subdir]. If \code{package} is missing, the package name is assumed to be equal the repository name and is extracted from \code{repo}.

For R packages which are not available on Bioconductor, CRAN or GitHub use \code{Rpackage}.
}

\value{
  Markdown-formatted character vector containing a hyperlinked package name.
}

\author{
  Andrzej Oleś <andrzej.oles@embl.de>, 2014-2015
}

\examples{

## link to a Bioconductor package
Biocpkg("IRanges")

## link to a CRAN package
CRANpkg("data.table")

## link to an R package on GitHub
Githubpkg("rstudio/rmarkdown")

}

\keyword{ manip }
