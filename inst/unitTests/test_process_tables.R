test_process_tables_wraps_body_tables <- function() {
    x <- c("<html><body>", "<table class=\"t\">", "<tr><td>1</td></tr>",
           "</table>", "<table>", "</table>", "</body></html>")
    y <- BiocStyle:::process_tables(x)
    checkEquals(length(y), length(x))
    checkEquals(lengths(regmatches(y, gregexpr("horizontal-scroll", y))),
                c(0L, 1L, 0L, 0L, 1L, 0L, 0L))
    checkEquals(sum(grepl("</table></div>", y, fixed = TRUE)), 2L)
}

test_process_tables_leaves_scripts_alone <- function() {
    ## Regression test for #97: htmlwidgets such as Glimma embed HTML
    ## strings (including <table>) in <script> blocks.
    script <- c("<script type=\"application/json\" data-for=\"w\">",
                "{\"html\":\"<table class='x'></table>\"}",
                "</script>",
                "<SCRIPT>var s = '<table>' + '</table>';</SCRIPT>")
    x <- c("<html><head>", script[4], "</head><body class=\"main\">",
           script[1:3], "<table>", "</table>", "</body></html>")
    y <- BiocStyle:::process_tables(x)
    checkEquals(y[c(2, 4:6)], x[c(2, 4:6)])
    checkEquals(y[7], "<div class='horizontal-scroll'><table>")
    checkEquals(y[8], "</table></div>")
}

test_process_tables_ignores_table_prefixed_tags <- function() {
    x <- "<tablefoo>not a table</tablefoo>"
    checkEquals(BiocStyle:::process_tables(x), x)
}
