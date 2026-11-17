## Install TinyTeX plus the LaTeX packages Bioconductor.sty and the
## pandoc LaTeX templates need. Shared by all CI workflows. The package
## list ships with BiocStyle; read it from the checkout (run from the repo
## root) so CI tests the same file users install from.
if (!tinytex::is_tinytex()) tinytex::install_tinytex(force = TRUE)
pkgs <- readLines("inst/resources/tex/required-packages.txt")
pkgs <- trimws(sub("#.*", "", pkgs))
tinytex::tlmgr_install(pkgs[nzchar(pkgs)])

## Put TinyTeX's binaries on PATH for later steps (Sweave/texi2dvi look on
## PATH; rmarkdown finds TinyTeX on its own).
gh_path <- Sys.getenv("GITHUB_PATH")
if (nzchar(gh_path)) {
    bin <- list.dirs(file.path(tinytex::tinytex_root(), "bin"),
                     recursive = FALSE)[1]
    cat(bin, "\n", file = gh_path, append = TRUE, sep = "")
}
