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


### print file content

.print.file <- function(file, scoped = FALSE) {
  type = unlist(strsplit(file, split=".", fixed=TRUE))
  type = tolower(type[length(type)])
  
  paste(c(
    switch(type, 
           js  = '<script type="text/javascript">',
           css = if (isTRUE(scoped)) '<style type="text/css" scoped>'
                 else '<style type="text/css">',
           NULL),
    ## actual file content
    readLines(file),
    switch(type, 
           js  = '</script>\n',
           css = '</style>\n',
           NULL)
    ), collapse = '\n')
}


## macro definitions

Biocpkg <- function(pkg) {
    Rpackage( sprintf("[%s](http://bioconductor.org/packages/%s)", pkg, pkg) )
}

Biocannopkg <- function(pkg) {
    Biocpkg(pkg)
}

Biocexptpkg <- function(pkg) {
    Biocpkg(pkg)
}

CRANpkg <- function(pkg) {
    cran.repository <- "http://cran.fhcrc.org/web/packages"
    fmt <- '[%s](%s/%s/index.html)'
    Rpackage( sprintf(fmt, pkg, cran.repository, pkg) )
}

Rpackage <- function(pkg) {
    sprintf('*%s*', pkg)
}

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

pkg_ver <- function(pkg) {
  paste(pkg, packageVersion(pkg))
}

doc_date <- function() {
  sub("^0", "", format(Sys.Date(), '%d %B %Y'))
}

## 

output_format <- function() {
  output = rmarkdown::metadata$output
  if (is.list(output)) output = names(output)[[1L]]
  output = regmatches(output, regexpr("html|pdf|word|md", output))[1L]
}
