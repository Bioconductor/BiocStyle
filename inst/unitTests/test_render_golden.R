## Golden-output (structural) render tests for BiocStyle::html_document and
## BiocStyle::pdf_document.
##
## The fixture cases/golden.Rmd (with cases/golden.bib) exercises the BiocStyle
## R Markdown features. It is copied to a temporary directory and rendered
## there, and the output is checked for the markup BiocStyle is responsible
## for. These are not pixel tests: they assert on structure, so they should
## survive cosmetic changes but catch template and post-processing breakage
## (e.g. after a pandoc or rmarkdown upgrade).
##
## Each output format is rendered once per test file run and cached in
## .golden_env; every file is written below tempdir().

.golden_env <- new.env(parent = emptyenv())

.golden_case_files <- function() {
    dir <- system.file("unitTests", "cases", package = "BiocStyle")
    file.path(dir, c("golden.Rmd", "golden.bib"))
}

## Copy the fixture into a fresh temporary directory, optionally rewriting its
## lines with 'transform', render it with 'output_format' and return the path
## of the output file.
.golden_render <- function(output_format, transform = NULL) {
    dir <- tempfile("BiocStyle-golden-")
    dir.create(dir)
    stopifnot(all(file.copy(.golden_case_files(), dir)))
    input <- file.path(dir, "golden.Rmd")
    if (is.function(transform))
        BiocStyle:::writeUTF8(transform(BiocStyle:::readUTF8(input)), input)
    rmarkdown::render(input, output_format = output_format,
                      output_dir = dir, intermediates_dir = dir,
                      quiet = TRUE, envir = new.env())
}

.golden_html <- function() {
    if (is.null(.golden_env$html)) {
        output <- .golden_render(BiocStyle::html_document())
        lines <- BiocStyle:::readUTF8(output)
        .golden_env$html <- list(
            lines = lines,
            ## the document body only, so that matches inside the embedded
            ## CSS and JavaScript in <head> do not count
            body = paste(lines[seq(grep("<body", lines)[1L], length(lines))],
                         collapse = "\n")
        )
    }
    .golden_env$html
}

.golden_has_latex <- function() {
    (requireNamespace("tinytex", quietly = TRUE) && tinytex::is_tinytex()) ||
        nzchar(Sys.which("pdflatex"))
}

## Text of a PDF with whitespace collapsed, or NULL if no extractor is
## available.
.golden_pdf_text <- function(pdf) {
    txt <- if (requireNamespace("pdftools", quietly = TRUE)) {
        pdftools::pdf_text(pdf)
    } else if (nzchar(Sys.which("pdftotext"))) {
        system2("pdftotext", c(shQuote(pdf), "-"), stdout = TRUE)
    } else {
        return(NULL)
    }
    gsub("[[:space:]]+", " ", paste(txt, collapse = " "))
}

.golden_pdf <- function() {
    if (is.null(.golden_env$pdf)) {
        pdf <- .golden_render(BiocStyle::pdf_document(keep_tex = TRUE))
        tex <- BiocStyle:::readUTF8(sub("[.]pdf$", ".tex", pdf))
        .golden_env$pdf <- list(
            pdf = pdf,
            tex = paste(tex, collapse = "\n"),
            text = .golden_pdf_text(pdf)
        )
    }
    .golden_env$pdf
}

.golden_package_line <- function()
    paste("BiocStyle", packageVersion("BiocStyle"))

.golden_check_match <- function(pattern, x, what, fixed = FALSE)
    checkTrue(grepl(pattern, x, fixed = fixed, perl = !fixed),
              msg = sprintf("%s not found (pattern: %s)", what, pattern))

## HTML ---------------------------------------------------------------------

test_html_golden_title_block <- function() {
    body <- .golden_html()$body

    .golden_check_match(
        '<h1 class="title[^"]*">Golden Render Fixture</h1>', body, "title")
    .golden_check_match(
        '<h3 class="subtitle">\\s*Structural Reference Document', body,
        "subtitle")

    ## authors with affiliation/email marks, as built by auth_affil_html()
    .golden_check_match(paste0(
        '<p class="author-name">Ada Alpha<span class="affil-mark">1\\*</span>, ',
        'Bram Beta<span class="affil-mark">2</span> and ',
        'Cleo Gamma<span class="affil-mark">1,3\\*\\*</span></p>'),
        body, "author names with affiliation marks")
    .golden_check_match(paste0(
        '<p class="author-affiliation">',
        '<span class="affil-mark">1</span>Institute of Alpha Studies<br>',
        '<span class="affil-mark">2</span>Beta University<br>',
        '<span class="affil-mark">3</span>Gamma Research Center</p>'),
        body, "numbered affiliations")
    for (email in c("ada.alpha@example.org", "cleo.gamma@example.org"))
        .golden_check_match(
            sprintf('<a href="mailto:%s">%s</a>', email, email),
            body, paste("email link", email), fixed = TRUE)
    .golden_check_match('<p class="author-email">', body, "author email block",
                        fixed = TRUE)

    .golden_check_match(
        '<h4 class="abstract">Abstract</h4>\\s*<p>This abstract marks',
        body, "abstract")

    ## 'package:' field gets the installed version appended by pre_processor
    .golden_check_match(
        sprintf('<h4 class="package">Package</h4>\\s*<p>%s</p>',
                .golden_package_line()),
        body, "package version line")
}

test_html_golden_toc <- function() {
    body <- .golden_html()$body
    .golden_check_match('<h1>Contents</h1>\\s*<div id="TOC"', body,
                        "TOC with Contents heading")
    toc <- regmatches(body, regexpr('(?s)<div id="TOC".*?</div>', body, perl = TRUE))
    for (id in c("package-macros", "code", "figures", "tables",
                 "notes-and-citations"))
        .golden_check_match(sprintf('href="#%s"', id), toc,
                            paste("TOC entry", id), fixed = TRUE)
}

test_html_golden_figures <- function() {
    body <- .golden_html()$body
    imgs <- regmatches(body, gregexpr("<img [^>]*>", body))[[1L]]
    checkEquals(3L, length(imgs), msg = "number of figures")

    ## fig.small / fig.wide set the img class via the html_document plot hook
    ## and every figure defaults to out.width = "100%"
    checkTrue(all(grepl('width="100%"', imgs, fixed = TRUE)),
              msg = 'all figures have width="100%"')
    checkTrue(!grepl("class=", imgs[1L], fixed = TRUE),
              msg = "default figure has no size class")
    .golden_check_match('class="smallfigure"', imgs[2L], "fig.small class",
                        fixed = TRUE)
    .golden_check_match('class="widefigure"', imgs[3L], "fig.wide class",
                        fixed = TRUE)

    ## numbered captions with the first sentence emphasized (titlecaps)
    for (cap in c("1: <span class=\"caption-title\">Regular figure</span>",
                  "2: <span class=\"caption-title\">Small figure</span>",
                  "3: <span class=\"caption-title\">Wide figure</span>"))
        .golden_check_match(cap, body, paste("figure caption", cap),
                            fixed = TRUE)
}

test_html_golden_table <- function() {
    body <- .golden_html()$body
    ## process_tables() wraps every table in a horizontal-scroll container
    .golden_check_match(paste0(
        "<div class='horizontal-scroll'><table[^>]*>\\s*",
        "<caption>.*?<span class=\"caption-title\">Fruit prices</span>"),
        body, "table in horizontal-scroll div with caption title")
    .golden_check_match("</table></div>", body, "closing table wrapper",
                        fixed = TRUE)
}

test_html_golden_sidenote <- function() {
    body <- .golden_html()$body
    ## process_footnotes() turns footnotes into margin sidenotes
    .golden_check_match(paste0(
        '<label for="sidenote-1" class="sidenote-number">1</label>',
        '<label for="sidenote-1" class="margin-toggle"[^>]*>1</label>',
        '<span class="sidenote" id="sidenote-1">',
        '<span class="sidenote-number">1</span> golden side note text</span>'),
        body, "sidenote markup")
    checkTrue(!grepl('class="footnotes', body, fixed = TRUE),
              msg = "footnotes block removed")
    checkTrue(!grepl('class="footnote-ref"', body, fixed = TRUE),
              msg = "footnote references replaced")
}

test_html_golden_citation <- function() {
    body <- .golden_html()$body
    ## link-citations is switched on by pre_processor
    .golden_check_match(
        '<span class="citation">\\(<a href="#ref-golden2024"[^>]*>Delta 2024</a>\\)',
        body, "linked citation")
    .golden_check_match('id="ref-golden2024"', body, "bibliography entry",
                        fixed = TRUE)
}

test_html_golden_macros <- function() {
    body <- .golden_html()$body
    .golden_check_match(
        '<em><a href="https://bioconductor.org/packages/[0-9.]+/IRanges">IRanges</a></em>',
        body, "Biocpkg link to bioconductor.org/packages")
    .golden_check_match(
        '<em><a href="https://CRAN.R-project.org/package=data.table">data.table</a></em>',
        body, "CRANpkg link", fixed = TRUE)
    .golden_check_match(
        '<em><a href="https://github.com/rstudio/rmarkdown">rmarkdown</a></em>',
        body, "Githubpkg link", fixed = TRUE)
}

test_html_golden_code_chunk <- function() {
    body <- .golden_html()$body
    .golden_check_match('<pre class="[^"]*\\br\\b[^"]*"><code[^>]*>', body,
                        "R source block")
    .golden_check_match("golden_sum", body, "chunk source", fixed = TRUE)
    .golden_check_match("## [1] 55", body, "chunk output", fixed = TRUE)
}

test_html_golden_css <- function() {
    ## self-contained output inlines bioconductor.css; every selector line of
    ## the stylesheet must be present
    css <- BiocStyle:::readUTF8(BiocStyle:::bioconductor.css)
    selectors <- unique(trimws(grep("[{][[:space:]]*$", css, value = TRUE)))
    checkTrue(length(selectors) > 0L, msg = "bioconductor.css has selectors")
    missing <- setdiff(selectors, trimws(.golden_html()$lines))
    checkTrue(length(missing) == 0L,
              msg = paste("bioconductor.css selectors missing from HTML:",
                          paste(missing, collapse = "; ")))
}

## PDF ----------------------------------------------------------------------

test_pdf_golden <- function() {
    if (!.golden_has_latex()) {
        message("test_pdf_golden: no LaTeX installation found, skipping")
        return(invisible())
    }
    out <- .golden_pdf()
    checkTrue(file.exists(out$pdf), msg = "PDF output exists")
    tex <- out$tex

    ## LaTeX intermediate (keep_tex = TRUE)
    .golden_check_match("\\\\RequirePackage\\[[^]]*\\]\\{[^}]*Bioconductor\\}",
                        tex, "Bioconductor.sty usage")
    .golden_check_match("\\bioctitle[]{Golden Render Fixture}", tex,
                        "\\bioctitle", fixed = TRUE)
    .golden_check_match("\\subtitle{Structural Reference Document}", tex,
                        "\\subtitle", fixed = TRUE)
    for (author in c(
        "\\author[1]{Ada Alpha\\thanks{\\ttfamily ada.alpha@example.org}}",
        "\\author[2]{Bram Beta}",
        "\\author[1,3]{Cleo Gamma\\thanks{\\ttfamily cleo.gamma@example.org}}",
        "\\affil[1]{Institute of Alpha Studies}",
        "\\affil[2]{Beta University}",
        "\\affil[3]{Gamma Research Center}"))
        .golden_check_match(author, tex, author, fixed = TRUE)
    .golden_check_match(
        sprintf("\\packageVersion{%s}", .golden_package_line()), tex,
        "\\packageVersion", fixed = TRUE)
    .golden_check_match("\\tableofcontents", tex, "\\tableofcontents",
                        fixed = TRUE)
    .golden_check_match(
        "\\\\href\\{https://bioconductor[.]org/packages/[0-9.]+/IRanges\\}\\{IRanges\\}",
        tex, "Biocpkg link")
    for (env in c("smallfigure", "figure*"))
        .golden_check_match(sprintf("\\begin{%s}", env), tex,
                            paste(env, "environment"), fixed = TRUE)
    .golden_check_match("\\footnote{golden side note text}", tex, "footnote",
                        fixed = TRUE)

    ## extracted PDF text, when a text extractor is available
    txt <- out$text
    if (is.null(txt)) {
        message("test_pdf_golden: no pdftools or pdftotext, ",
                "skipping PDF text checks")
        return(invisible())
    }
    for (s in c("Golden Render Fixture", "Ada Alpha", "Bram Beta",
                "Cleo Gamma", "Institute of Alpha Studies", "Beta University",
                "Gamma Research Center", "This abstract marks",
                paste("Package", .golden_package_line()), "Contents",
                "Figure 2: Small figure", "Table 1: Fruit prices"))
        .golden_check_match(s, txt, paste("PDF text", sQuote(s)), fixed = TRUE)
}

test_pdf_golden_subtitle_text <- function() {
    DEACTIVATED(paste(
        "Bioconductor.sty redefines \\maketitle without titling's \\posttitle,",
        "where the template's \\subtitle stores the subtitle, so it is never",
        "typeset"))
    if (!.golden_has_latex()) return(invisible())
    txt <- .golden_pdf()$text
    if (is.null(txt)) return(invisible())
    .golden_check_match("Structural Reference Document", txt,
                        "PDF text subtitle", fixed = TRUE)
}

test_pdf_golden_citations <- function() {
    ## Regression test for #94 (pandoc >= 3.1.8 citeproc output)
    if (!.golden_has_latex()) return(invisible())
    txt <- .golden_pdf()$text
    if (!is.null(txt))
        .golden_check_match("Delta", txt, "PDF text citation", fixed = TRUE)
}
