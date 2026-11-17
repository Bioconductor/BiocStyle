## number of <img> tags with the given attribute; the embedded CSS also
## mentions width="100%", so count tags rather than lines
.count_img_attr <- function(html, attr)
    sum(grepl(attr, regmatches(html, gregexpr("<img [^>]*>", html))[[1L]],
              fixed = TRUE))

test_default_figure_options <- function() {
    filename <- rmarkdown::draft(tempfile(fileext = ".Rmd"), edit = FALSE,
                                 template="html_document", package = "BiocStyle")
    output_file <- tempfile(fileext = ".html")
    rmarkdown::render(filename, output_file = output_file, quiet = TRUE)

    ## with default options, we expect all three figures to have out.width = "100%"
    html <- paste(BiocStyle:::readUTF8(output_file), collapse = "\n")
    checkEquals(3L, .count_img_attr(html, 'width="100%"'))
    file.remove(output_file)
    
    ## modify the small figure to have a define out.width and check it's respected
    lines <- BiocStyle:::readUTF8(filename)
    lines <- gsub(x = lines, pattern = "fig.small=TRUE", 
                  replacement = 'fig.small=TRUE, out.width="500px"')
    BiocStyle:::writeUTF8(lines, filename)
    rmarkdown::render(filename, output_file = output_file, quiet = TRUE)
    html <- paste(BiocStyle:::readUTF8(output_file), collapse = "\n")
    checkEquals(2L, .count_img_attr(html, 'width="100%"'))
    checkEquals(1L, .count_img_attr(html, 'width="500px"'))
    file.remove(output_file)
    
    file.remove(filename)
}


