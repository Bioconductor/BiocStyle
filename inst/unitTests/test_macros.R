test_Biocpkg_versioned <- function() {
    v <- as.character(BiocManager::version())
    checkTrue(grepl(sprintf("bioconductor.org/packages/%s/IRanges", v),
                    BiocStyle::Biocpkg("IRanges"), fixed = TRUE))
    unv <- BiocStyle::Biocpkg("IRanges", versioned = FALSE)
    checkTrue(grepl("(https://bioconductor.org/packages/IRanges)", unv,
                    fixed = TRUE))
    vig <- BiocStyle::Biocpkg("IRanges", vignette = "IRangesOverview.html",
                              versioned = FALSE)
    checkTrue(grepl("packages/IRanges/vignettes/IRangesOverview.html", vig,
                    fixed = TRUE))
    checkException(BiocStyle::Biocpkg("IRanges", versioned = NA),
                   silent = TRUE)
}

test_Biocbook_versioned <- function() {
    v <- as.character(BiocManager::version())
    checkTrue(grepl(sprintf("https://bioconductor.org/books/%s/OSCA", v),
                    BiocStyle::Biocbook("OSCA"), fixed = TRUE))
    checkTrue(grepl("(https://bioconductor.org/books/OSCA)",
                    BiocStyle::Biocbook("OSCA", versioned = FALSE),
                    fixed = TRUE))
}
