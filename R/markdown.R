markdown <-
    function(css.files, self.contained = TRUE, links.target = TRUE)
{
    ## load the package to expose macros
    require(BiocStyle, quietly = TRUE)
    
    ## correct title margin (this has to be done from JS and not CSS
    ## becasue of differences in tag layout between markdown v1 and
    ## v2)
    cat( .print.js(system.file(package = "BiocStyle", "resources",
                               "html", "setTitleMargin.js")) )

    ## set up target of external links
    if ( isTRUE(links.target) )
        cat( .print.js(system.file(package = "BiocStyle", "resources",
                                   "html", "setExternalLinksTarget.js")) )
    
    ## set up CSS
    
    ## get the version of markdown renderer

    rmarkdown.version <-
        if ( is.null(knitr::opts_knit$get("rmarkdown.version")) ) {
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
        cat(.print.css(.bioconductor.css, scoped = TRUE))
    else
        options(markdown.HTML.stylesheet = .bioconductor.css)
    
    if ( !missing(css.files) ) {
        ## fail save
        css.files = css.files[file.exists(css.files)]

        if ( length(css.files) > 0 ) {
            if (insert.into.body)
                cat(.print.css(css.files, scoped = TRUE))      
            else
                options(markdown.HTML.header = if(isTRUE(self.contained)) {
                    ## insert the contents of the CSS files into the
                    ## HTML document
                    sapply(css.files, .print.css)
                } else {
                    ## insert relative links to the .css files
                    txt <- '<link rel="stylesheet" type="text/css" href="./%s"/>'
                    sprintf(txt, css.files)
                })
        }
    }
    
    invisible()
}


### print file content

.print.file <- function(x) {
    paste(readLines(x), collapse = '\n')
}

.print.css <- function(x, scoped = FALSE) {
    prefix <- if (scoped) {
        '<style type="text/css" scoped>'
    } else '<style type="text/css">'
    paste(prefix, .print.file(x), '</style>\n', sep = "\n")
}

.print.js <- function(x) {
    paste('<script type="text/javascript">', .print.file(x),
          '</script>\n', sep = "\n")
}


## macros definitions

.release.repository <- "http://bioconductor.org/packages/release"

Biocpkg <- function(pkg) {
    fmt <- '[%s](%s/bioc/html/%s.html)'
    Rpackage( sprintf(fmt, pkg, .release.repository, pkg) )
}

Biocannopkg <- function(pkg) {
    fmt <- '[%s](%s/data/annotation/html/%s.html)'
    Rpackage( sprintf(fmt, pkg, .release.repository, pkg) )
}

Biocexptpkg <- function(pkg) {
    fmt <- '[%s](%s/data/experiment/html/%s.html)'
    Rpackage( sprintf(fmt, pkg, .release.repository, pkg) )
}

CRANpkg <- function(pkg) {
    cran.repository <- "http://cran.fhcrc.org/web/packages"
    fmt <- '[%s](%s/%s/index.html)'
    Rpackage( sprintf(fmt, pkg, cran.repository, pkg) )
}

Rpackage <- function(pkg) {
    sprintf('*%s*', pkg)
}

Githubpkg <- function(pkg) {
    github <- "https://github.com"
    pkg = strsplit(pkg, split = "/", fixed = TRUE)[[1]]
    Rpackage( sprintf('[%s](%s/%s/%s)', pkg[2], github, pkg[1], pkg[2]) )
}
