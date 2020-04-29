To update the vignette

1. Modify `skeleton.Rmd.whisker`

2. Use `BiocStyle::use_vignette_html()` to update `skeleton/skeleton.Rmd`

    ```{r}
    devtools::load_all() # in BiocStyle source directory
    destination <- file.path(
        "inst", "rmarkdown", "templates", "package_vignette", "skeleton",
        "skeleton.Rmd"
    )
    BiocStyle::use_vignette_html(destination)
    ```

These steps keep the vignette produced by `use_vignette_html()` in
sync with selecting 'Bioconductor package vignette' from the RStudio
`File -> New File -> R Markdown -> From Template` menu.
