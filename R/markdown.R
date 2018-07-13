#' Use Bioconductor CSS style to format HTML vignettes
#' 
#' This function sets the Bioconductor style sheet to provide a consistent
#' style across Bioconductor HTML vignettes.
#' 
#' Use is described in the \sQuote{Bioconductor CSS Style} vignette.
#' 
#' @param css.files character vector containing the location of additional
#' \code{.css} files.
#' @param self.contained logical(1), should the content of the CSS
#' \code{stylesheet} files be included into the html file or should they be
#' saved as separate files.
#' @param links.target logical(1), should external links open in new browser
#' tab/window.
#' @return No value is returned. The function is called for its side effect of
#' setting the \code{markdown} and/or \code{knitr} specific options controlling
#' the inclusion of the Bioconductor CSS style file in the HTML output.
#' @author Andrzej Oleś <andrzej.oles@@embl.de>, 2014-2015
#' @keywords manip
#' @examples
#' 
#' ## location of the .css file
#' BiocStyle:::bioconductor.css
#' 
#' @export
markdown <-
    function(css.files, self.contained = TRUE, links.target = TRUE)
{
    ## load the package to expose macros
    require(BiocStyle, quietly = TRUE)
    
    ## set document title class as in R Markdown v2
    cat( .print.file(file.path(resources, "html", "setDocumentTitleClass.js")) )

    ## set up target of external links
    if ( isTRUE(links.target) )
        cat( .print.file(file.path(resources, "html", "setExternalLinksTarget.js")) )
    
    ## set up CSS
    
    ## get the version of markdown renderer

    rmarkdown.version <-
        if ( is.null(opts_knit$get("rmarkdown.version")) ) {
            1L
        } else 2L
    
    ## in case of rmarkdown v1 insert directly into document body to
    ## circumvent issues with preview in R Studio, unless called from
    ## 'knit2html' or the vignette builder
    calls <- sapply(sys.calls(), function(x) as.character(x)[1])
    pattern <- c("knit2html", "buildVignettes")
    
    insert.into.body <-
        rmarkdown.version == 1L && all(sapply(pattern, function(x) {
            length(grep(x, calls, fixed = TRUE))
        }) == 0)
    
    if (insert.into.body)
        cat(.print.file(bioconductor.css, scoped = TRUE))
    else
        options(markdown.HTML.stylesheet = bioconductor.css)
    
    if ( !missing(css.files) ) {
        ## fail save
        css.files = css.files[file.exists(css.files)]

        if ( length(css.files) > 0 ) {
            if (insert.into.body)
                cat(.print.file(css.files, scoped = TRUE))      
            else
                options(markdown.HTML.header = if(isTRUE(self.contained)) {
                    ## insert the contents of the CSS files into the
                    ## HTML document
                    sapply(css.files, .print.file)
                } else {
                    ## insert relative links to the .css files
                    txt <- '<link rel="stylesheet" type="text/css" href="./%s"/>'
                    sprintf(txt, css.files)
                })
        }
    }
    
    invisible()
}


## macro definitions

#' Link to packages on Bioconductor, CRAN and GitHub
#' 
#' Functions for adding links to Bioconductor, CRAN and GitHub packages into R
#' Markdown documents.
#' 
#' Use \code{Biocpkg} for Bioconductor software, annotation and experiment data
#' packages. The function automatically includes a link to the package landing
#' page, the version of which depends on the current Bioconductor version (i.e.
#' if run in a devel environment, it will point towards the devel landing page;
#' otherwise it will point to the release landing page).
#'
#' Use \code{Biocworkpkg} for Bioconductor workflow packages. This function
#' automatically includes a link to the package landing page. If 
#' \code{vignette} is specified, the function returns the address of the 
#' compiled vignette document. 
#' 
#' Use \code{CRANpkg} for R packages available on CRAN. The function
#' automatically includes a link to the master CRAN landing page.
#' 
#' Use \code{Githubpkg} for R packages available on GitHub. The \code{repo}
#' should include the repository address in the format username/repo[/subdir].
#' If \code{package} is missing, the package name is assumed to be equal the
#' repository name and is extracted from \code{repo}.
#' 
#' For R packages which are not available on Bioconductor, CRAN or GitHub, use
#' \code{Rpackage}.
#' 
#' @param pkg character(1), package name
#' @param vignette character(1), vignette name for workflows
#' @param repo Repository address in the format username/repo[/subdir]
#' @return Markdown-formatted character vector containing a hyperlinked package
#' name.
#' 
#' For \code{Biocworkpkg} with \code{vignette!=NULL}, an address to the 
#' compiled vignette (optionally a specific section) is returned.
#'
#' @author Andrzej Oleś <andrzej.oles@@embl.de>, 2014-2015
#' @examples
#' 
#' 
#' ## link to a Bioconductor software package
#' Biocpkg("IRanges")
#' 
#' ## link to a Bioconductor annotation package
#' Biocannopkg("org.Mm.eg.db")
#'
#' ## link to a Bioconductor experiment data package
#' Biocexptpkg("affydata")
#' 
#' ## link to a Bioconductor workflow
#' Biocworkpkg("simpleSingleCell")
#' Biocworkpkg("simpleSingleCell", vignette="work-0-intro")
#'
#' ## link to a CRAN package
#' CRANpkg("data.table")
#' 
#' ## link to an R package on GitHub
#' Githubpkg("rstudio/rmarkdown")
#'
#' @name macros
NULL

#' @rdname macros
#' @export
Biocpkg <- function(pkg) {
    url <- "https://bioconductor.org/packages/%s/bioc/html/%s.html"
    url <- sprintf(url, is_devel(), pkg) 
    Rpackage( sprintf("[%s](%s)", pkg, url) )
}

#' @rdname macros
#' @export
Biocannopkg <- function(pkg) {
    url <- "https://bioconductor.org/packages/%s/data/annotation/html/%s.html"
    url <- sprintf(url, is_devel(), pkg) 
    Rpackage( sprintf("[%s](%s)", pkg, url) )
}

#' @rdname macros
#' @export
Biocexptpkg <- function(pkg) {
    url <- "https://bioconductor.org/packages/%s/data/experiment/html/%s.html"
    url <- sprintf(url, is_devel(), pkg) 
    Rpackage( sprintf("[%s](%s)", pkg, url) )
}

#' @rdname macros
#' @export
Biocworkpkg <- function(pkg, vignette=NULL) {
    mode <- is_devel()

    if (is.null(vignette)) {
        url <- "https://bioconductor.org/packages/%s/workflows/html/%s.html"
        url <- sprintf(url, mode, pkg)
        return(Rpackage( sprintf("[%s](%s)", pkg, url) ))
    }

    url <- sprintf(file.path("https://bioconductor.org/packages/%s/workflows",
        "vignettes/%s/inst/doc/%s.html"), mode, pkg, vignette)
    return(url)
}

#' @import BiocVersion
#' @importFrom utils packageVersion
is_devel <- function() {
    as.character(packageVersion("BiocVersion")[,1:2])
}

#' @rdname macros
#' @export
CRANpkg <- function(pkg) {
    cran <- "https://CRAN.R-project.org/package"
    fmt <- '[%s](%s=%s)'
    Rpackage( sprintf(fmt, pkg, cran, pkg) )
}

#' @rdname macros
#' @export
Rpackage <- function(pkg) {
    sprintf('*%s*', pkg)
}

#' @rdname macros
#' @export
Githubpkg <- function(repo, pkg) {
    github <- "https://github.com"
    ## extract package name
    if (missing(pkg)) {
      names = strsplit(repo, split = "/", fixed = TRUE)[[1L]]
      pkg = names[length(names)]
    }
    Rpackage( sprintf('[%s](%s/%s)', pkg, github, repo) )
}


## yaml header convenience functions

#' Specify Rmarkdown document metadata
#' 
#' Helper functions for including metadata in the document header.
#' 
#' Use \code{doc_date} to include document compilation date in the document
#' metadata field 'date', and \code{pkg_ver} for package version specification
#' in the 'package' field.
#' 
#' @param pkg character(1), package name
#' @return Markdown-formatted character string.
#' @author Andrzej Oleś <andrzej.oles@@embl.de>, 2014-2015
#' @keywords manip
#' @examples
#' 
#' 
#' ## current date
#' doc_date()
#' 
#' ## package name with version
#' pkg_ver("BiocStyle")
#' 
#' @name helpers
NULL

#' @rdname helpers
#' @export
pkg_ver <- function(pkg) {
  pkgVer <- if (pkg=="packageName") "X.Y.Z"  else packageVersion(pkg)
  paste(pkg, pkgVer)
}

#' @rdname helpers
#' @export
doc_date <- function() {
  sub("^0", "", format(Sys.Date(), '%d %B %Y'))
}


#' Output format of an R Markdown document
#' 
#' Helper function to determine the document's current pandoc output format.
#' 
#' The function is useful for defining different behavior depending on the
#' output format, e.g. figure settings.
#' 
#' @return A character string specifying the pandoc output format.
#' @author Andrzej Oleś <andrzej.oles@@embl.de>, 2016
#' @examples
#' 
#' \dontrun{
#' ## Switch between SVG and PDF figures depending on document output format
#' knitr::opts_chunk$set(
#'   dev = switch(output(), html = "svg", latex = "pdf")
#'   )
#' }
#' 
#' @export output
output = function () {
  opts_knit$get("rmarkdown.pandoc.to")
}
