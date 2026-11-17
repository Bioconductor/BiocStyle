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
