## Install TinyTeX plus the LaTeX packages Bioconductor.sty and the
## pandoc LaTeX templates need. Shared by all CI workflows.
if (!tinytex::is_tinytex()) tinytex::install_tinytex(force = TRUE)
pkgs <- c(
    "bera", "caption", "changepage", "enumitem", "etoolbox",
    "everysel", "fancyhdr", "footmisc", "framed", "geometry", "marginfix",
    "mathtools", "nowidow", "parnotes", "parskip", "pdfcrop", "placeins",
    "preprint", "ragged2e", "sectsty", "silence", "soul", "titlesec",
    "titling", "xcolor", "xstring"
)
tinytex::tlmgr_install(pkgs)

## Put TinyTeX's binaries on PATH for later steps (Sweave/texi2dvi look on
## PATH; rmarkdown finds TinyTeX on its own).
gh_path <- Sys.getenv("GITHUB_PATH")
if (nzchar(gh_path)) {
    bin <- list.dirs(file.path(tinytex::tinytex_root(), "bin"),
                     recursive = FALSE)[1]
    cat(bin, "\n", file = gh_path, append = TRUE, sep = "")
}
