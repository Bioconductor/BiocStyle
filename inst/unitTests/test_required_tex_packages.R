## inst/resources/tex/required-packages.txt lists the TeX Live packages that
## BiocStyle needs (#15). These tests keep it in sync with Bioconductor.sty.

.required_tex_packages <- function() {
    fl <- system.file("resources", "tex", "required-packages.txt",
                      package = "BiocStyle")
    pkgs <- trimws(sub("#.*", "", readLines(fl)))
    pkgs[nzchar(pkgs)]
}

test_required_tex_packages_parse <- function() {
    pkgs <- .required_tex_packages()
    checkTrue(length(pkgs) > 0L)
    checkTrue(!anyDuplicated(pkgs))
    ## one TeX Live package name per line
    checkTrue(all(grepl("^[[:alnum:]-]+$", pkgs)))
}

test_required_tex_packages_cover_Bioconductor_sty <- function() {
    sty <- readLines(BiocStyle:::bioconductor.sty)
    sty <- sub("%.*", "", sty)
    ## \RequirePackage[opts]{name}; options may span lines (hyperref)
    sty <- paste(sty, collapse = "")
    latex <- regmatches(sty, gregexpr(
        "\\\\RequirePackage(\\[[^]]*\\])?\\{[^}]+\\}", sty))[[1]]
    latex <- sub(".*\\{([^}]+)\\}$", "\\1", latex)
    checkTrue("hyperref" %in% latex)
    ## LaTeX package -> TeX Live package, where the names differ
    tl_name <- c(authblk = "preprint", beramono = "bera", fontenc = "latex",
                 graphicx = "graphics", ifthen = "latex", titletoc = "titlesec")
    tl <- ifelse(latex %in% names(tl_name), tl_name[latex], latex)
    missing <- setdiff(tl, .required_tex_packages())
    checkIdentical(character(), missing,
                   paste("not in required-packages.txt:",
                         paste(missing, collapse = ", ")))
}

test_required_tex_packages_cover_template_and_crop <- function() {
    pkgs <- .required_tex_packages()
    ## titling is loaded by the pdf_document template, pdfcrop crops figures
    checkTrue(all(c("titling", "pdfcrop") %in% pkgs))
}
