## Author, affiliation and email handling in the title block of
## BiocStyle::html_document and BiocStyle::pdf_document (R/auth_affil.R).
##
## Regression tests for https://github.com/Bioconductor/BiocStyle/issues/53:
## affiliation marks are only shown when they are needed to tell affiliations
## apart, and the PDF output always shows the affiliations, including those
## of a single author given as a YAML map.

.auth_affil_yaml <- list(
    single_author_list = c(
        "author:",
        "- name: Ziyi Li",
        "  affiliation:",
        "  - Emory University"),
    single_author_map = c(
        "author:",
        "  name: Ziyi Li",
        "  affiliation: Emory University",
        "  email: zl@example.org"),
    shared_affiliation = c(
        "author:",
        "- name: Ann A",
        "  affiliation: Shared Institute",
        "  email: ann@example.org",
        "- name: Bob B",
        "  affiliation: Shared Institute"),
    different_affiliations = c(
        "author:",
        "- name: Ann A",
        "  affiliation: First Institute",
        "- name: Bob B",
        "  affiliation: Second Institute",
        "  email: bob@example.org"),
    two_affiliations = c(
        "author:",
        "- name: Ann A",
        "  affiliation:",
        "  - First Institute",
        "  - Second Institute")
)

## Front matter metadata as rmarkdown would pass it to the post-processors
.auth_affil_metadata <- function(case)
    yaml::yaml.load(paste(.auth_affil_yaml[[case]], collapse = "\n"))

## Render a document whose front matter has the author block of 'case' and
## return the path of the output file.
.auth_affil_render <- function(case, output_format) {
    dir <- tempfile("BiocStyle-auth-affil-")
    dir.create(dir)
    input <- file.path(dir, paste0(case, ".Rmd"))
    BiocStyle:::writeUTF8(c("---", "title: Author test",
                            .auth_affil_yaml[[case]], "---", "", "Text."),
                          input)
    rmarkdown::render(input, output_format = output_format,
                      output_dir = dir, intermediates_dir = dir,
                      quiet = TRUE, envir = new.env())
}

## The author and affiliation paragraphs of the rendered HTML title block
.auth_affil_html <- function(case) {
    lines <- BiocStyle:::readUTF8(
        .auth_affil_render(case, BiocStyle::html_document()))
    grep('^<p class="author-', lines, value = TRUE)
}

.auth_affil_has_latex <- function() {
    (requireNamespace("tinytex", quietly = TRUE) && tinytex::is_tinytex()) ||
        nzchar(Sys.which("pdflatex"))
}

## The \author, \affil and \setcounter{Maxaffil} lines of the LaTeX intermediate and the
## PDF text with whitespace collapsed (NULL without a text extractor), or NULL
## if there is no LaTeX installation.
.auth_affil_pdf <- function(case) {
    if (!.auth_affil_has_latex()) {
        message("no LaTeX installation found, skipping PDF checks")
        return(NULL)
    }
    pdf <- .auth_affil_render(case, BiocStyle::pdf_document(keep_tex = TRUE))
    tex <- BiocStyle:::readUTF8(sub("[.]pdf$", ".tex", pdf))
    txt <- if (requireNamespace("pdftools", quietly = TRUE)) {
        pdftools::pdf_text(pdf)
    } else if (nzchar(Sys.which("pdftotext"))) {
        system2("pdftotext", c("-layout", shQuote(pdf), "-"), stdout = TRUE)
    }
    if (!is.null(txt))
        txt <- gsub("[[:space:]]+", " ", paste(txt, collapse = " "))
    list(tex = grep("^\\\\(author|affil|setcounter[{]Maxaffil)", tex,
                    value = TRUE),
         text = txt)
}

.auth_affil_check_text <- function(pattern, txt, what) {
    if (is.null(txt)) {
        message("no pdftools or pdftotext, skipping PDF text check: ", what)
        return(invisible())
    }
    checkTrue(grepl(pattern, txt, perl = TRUE),
              msg = sprintf("PDF text %s (pattern: %s)", what, pattern))
}

## parse_auth_affil() --------------------------------------------------------

test_parse_auth_affil_marks <- function() {
    parse <- function(case)
        BiocStyle:::parse_auth_affil(.auth_affil_metadata(case))

    m <- parse("single_author_list")
    checkIdentical("Emory University", m$affil)
    checkTrue(is.null(m$affil_marks), msg = "single author: no marks")

    m <- parse("shared_affiliation")
    checkIdentical("Shared Institute", m$affil)
    checkTrue(is.null(m$affil_marks), msg = "shared affiliation: no marks")
    checkIdentical(c("*", ""), unname(m$email_marks))

    m <- parse("different_affiliations")
    checkIdentical(c("1", "2"), unname(m$affil_marks))

    m <- parse("two_affiliations")
    checkIdentical("1,2", unname(m$affil_marks))

    ## a single affiliation not shared by every author still needs its mark
    m <- BiocStyle:::parse_auth_affil(list(author = list(
        list(name = "Ann A", affiliation = "Only Institute"),
        list(name = "Bob B"))))
    checkIdentical(c("1", ""), unname(m$affil_marks))
}

## HTML ----------------------------------------------------------------------

test_html_auth_affil_single_author_list <- function() {
    checkIdentical(c(
        '<p class="author-name">Ziyi Li</p>',
        '<p class="author-affiliation">Emory University</p>'),
        .auth_affil_html("single_author_list"))
}

test_html_auth_affil_single_author_map <- function() {
    checkIdentical(c(
        '<p class="author-name">Ziyi Li</p>',
        '<p class="author-affiliation">Emory University</p>',
        paste0('<p class="author-email">',
               '<a href="mailto:zl@example.org">zl@example.org</a></p>')),
        .auth_affil_html("single_author_map"))
}

test_html_auth_affil_shared_affiliation <- function() {
    checkIdentical(c(
        paste0('<p class="author-name">Ann A<span class="affil-mark">*</span>',
               ' and Bob B</p>'),
        '<p class="author-affiliation">Shared Institute</p>',
        paste0('<p class="author-email"><span class="affil-mark">*</span>',
               '<a href="mailto:ann@example.org">ann@example.org</a></p>')),
        .auth_affil_html("shared_affiliation"))
}

test_html_auth_affil_different_affiliations <- function() {
    checkIdentical(c(
        paste0('<p class="author-name">Ann A<span class="affil-mark">1</span>',
               ' and Bob B<span class="affil-mark">2*</span></p>'),
        paste0('<p class="author-affiliation">',
               '<span class="affil-mark">1</span>First Institute<br>',
               '<span class="affil-mark">2</span>Second Institute</p>'),
        paste0('<p class="author-email"><span class="affil-mark">*</span>',
               '<a href="mailto:bob@example.org">bob@example.org</a></p>')),
        .auth_affil_html("different_affiliations"))
}

test_html_auth_affil_two_affiliations <- function() {
    checkIdentical(c(
        '<p class="author-name">Ann A<span class="affil-mark">1,2</span></p>',
        paste0('<p class="author-affiliation">',
               '<span class="affil-mark">1</span>First Institute<br>',
               '<span class="affil-mark">2</span>Second Institute</p>')),
        .auth_affil_html("two_affiliations"))
}

## PDF -----------------------------------------------------------------------

test_pdf_auth_affil_single_author_list <- function() {
    out <- .auth_affil_pdf("single_author_list")
    if (is.null(out)) return(invisible())
    checkIdentical(c("\\author{Ziyi Li}", "\\setcounter{Maxaffil}{\\maxdimen}",
                     "\\affil{Emory University}"), out$tex)
    .auth_affil_check_text("Ziyi Li Emory University", out$text,
                           "unmarked author and affiliation")
}

test_pdf_auth_affil_single_author_map <- function() {
    out <- .auth_affil_pdf("single_author_map")
    if (is.null(out)) return(invisible())
    checkIdentical(c("\\author{Ziyi Li\\thanks{\\ttfamily zl@example.org}}",
                     "\\setcounter{Maxaffil}{\\maxdimen}",
                     "\\affil{Emory University}"), out$tex)
    .auth_affil_check_text(
        "Ziyi Li \\W+ Emory University \\W+ zl@example\\.org", out$text,
        "author, affiliation and email")
}

test_pdf_auth_affil_shared_affiliation <- function() {
    out <- .auth_affil_pdf("shared_affiliation")
    if (is.null(out)) return(invisible())
    checkIdentical(c(
        "\\author{Ann A\\thanks{\\ttfamily ann@example.org} and Bob B}",
        "\\setcounter{Maxaffil}{\\maxdimen}",
        "\\affil{Shared Institute}"), out$tex)
    .auth_affil_check_text(
        "Ann A \\W+ and Bob B Shared Institute \\W+ ann@example\\.org",
        out$text, "authors with one unmarked shared affiliation")
}

test_pdf_auth_affil_different_affiliations <- function() {
    out <- .auth_affil_pdf("different_affiliations")
    if (is.null(out)) return(invisible())
    checkIdentical(c(
        "\\author[1]{Ann A}",
        "\\author[2]{Bob B\\thanks{\\ttfamily bob@example.org}}",
        "\\affil[1]{First Institute}", "\\affil[2]{Second Institute}"),
        out$tex)
    .auth_affil_check_text(paste0(
        "Ann A 1 and Bob B \\W+2 1 First Institute 2 Second Institute ",
        "\\W+ bob@example\\.org"), out$text, "marked authors and affiliations")
}

test_pdf_auth_affil_two_affiliations <- function() {
    out <- .auth_affil_pdf("two_affiliations")
    if (is.null(out)) return(invisible())
    checkIdentical(c("\\author[1,2]{Ann A}", "\\affil[1]{First Institute}",
                     "\\affil[2]{Second Institute}"), out$tex)
    .auth_affil_check_text("Ann A 1,2 1 First Institute 2 Second Institute",
                           out$text, "author with two marked affiliations")
}
