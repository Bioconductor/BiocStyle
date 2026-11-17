test_md_document_renders <- function() {
    dir <- tempfile(); dir.create(dir)
    rmd <- file.path(dir, "doc.Rmd")
    writeLines(c("---", 'title: "MD"', "output:", "  BiocStyle::md_document:",
                 "    toc: true", "---", "", "# Section One", "",
                 "Text with `r 1 + 1`.", "", "## Sub", "", "More."), rmd)
    out <- rmarkdown::render(rmd, quiet = TRUE)
    md <- readLines(out)
    ## the title becomes the level-1 heading, so sections shift down one level
    checkTrue(any(md == "# MD"), "title as ATX level-1 heading")
    checkTrue(any(md == "## Section One"), "ATX section heading")
    checkTrue(any(md == "### Sub"), "ATX subsection heading")
    checkTrue(any(md == "{:toc}"), "kramdown TOC marker")
    unlink(dir, recursive = TRUE)
}
