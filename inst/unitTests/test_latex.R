test_maketitle <- function()
{
    dir <- system.file("unitTests", "cases", package="BiocStyle")
    fls <- dir(dir, pattern="maketitle_test.*.Rnw", full=TRUE)

    fls <- normalizePath(fls)
    pwd <- setwd(tempdir())
    on.exit(setwd(pwd))

    for (fl in fls) {
        checkTrue(file.copy(fl, rnw <- file.path(tempdir(), basename(fl))))
        tex <- utils::Sweave(rnw)
        pdf <- tools::texi2pdf(tex)
    }
}
