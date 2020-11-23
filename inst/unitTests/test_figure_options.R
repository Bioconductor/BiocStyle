test_default_figure_options <- function() {
    filename <- rmarkdown::draft(tempfile(fileext = ".Rmd"), edit = FALSE,
                                 template="html_document", package = "BiocStyle")
    output_file <- tempfile(fileext = ".html")
    rmarkdown::render(filename, output_file = output_file, quiet = TRUE)

    ## with default options, we expect all three figures to have out.width = "100%"
    html <- BiocStyle:::readUTF8(output_file)
    checkTrue(length( grep(pattern = 'width="100%"', x = html) ) == 3L)
    file.remove(output_file)
    
    ## modify the small figure to have a define out.width and check it's respected
    lines <- BiocStyle:::readUTF8(filename)
    lines <- gsub(x = lines, pattern = "fig.small=TRUE", 
                  replacement = 'fig.small=TRUE, out.width="500px"')
    BiocStyle:::writeUTF8(lines, filename)
    rmarkdown::render(filename, output_file = output_file, quiet = TRUE)
    html <- BiocStyle:::readUTF8(output_file)
    checkTrue(length( grep(pattern = 'width="100%"', x = html) ) == 2L)
    checkTrue(length( grep(pattern = 'width="500px"', x = html) ) == 1L)
    file.remove(output_file)
    
    file.remove(filename)
}


