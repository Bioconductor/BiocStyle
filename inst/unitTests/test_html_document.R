## Unit tests for the BiocStyle::html_document post-processing helpers in
## R/html_document.R.

test_modify_css_removes_pre_code_padding <- function() {
    ## rmarkdown injects 'pre code { padding: 0; }', which overrides the code
    ## block padding in bioconductor.css. Pandoc < 3 keeps its indentation,
    ## pandoc >= 3 drops it when embedding the CSS (#103).
    for (rule in list(c("pre code {", "  padding: 0;", "}"),
                      c("pre code {", "padding: 0;", "}"))) {
        x <- c("<style>", "pre code {", "padding: 10px 10px 10px 0px;", "}",
               rule, "</style>")
        y <- BiocStyle:::modify_css(x)
        checkEquals(c("<style>", "pre code {", "padding: 10px 10px 10px 0px;",
                      "}", "</style>"), y,
                    msg = paste(rule, collapse = "\\n"))
    }
}

test_modify_css_keeps_other_pre_code_rules <- function() {
    ## bootstrap's minified rule and other 'pre code' rules are left alone
    x <- c("pre code{padding:0;font-size:inherit;color:inherit}",
           "pre code {", "white-space: pre;", "}")
    checkEquals(x, BiocStyle:::modify_css(x))
}
