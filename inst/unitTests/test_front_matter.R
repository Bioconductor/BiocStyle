test_partition_yaml_front_matter_matches_rmarkdown <- function() {
    cases <- list(
        c("---", "title: x", "---", "body", "more"),
        c("", "  ", "---", "title: x", "...", "body"),
        c("text", "---", "title: x", "---"),
        c("---", "---", "body"),
        c("---", "title: x", "---"),
        c("no front matter"),
        c("<!-- rnb-text-begin -->", "---", "a: 1", "---", "b"),
        character()
    )
    ref <- get("partition_yaml_front_matter", asNamespace("rmarkdown"))
    for (x in cases)
        checkIdentical(BiocStyle:::partition_yaml_front_matter(x), ref(x))
}
