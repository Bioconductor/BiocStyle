test_maketitle <- function()
{
    fls <- dir("cases", pattern="maketitle_test.*.Rnw", full=TRUE)
    fls <- normalizePath(fls)
    pwd <- setwd(tempdir())
    on.exit(setwd(pwd))

    for (fl in fls) {
        checkTrue(file.copy(fl, rnw <- tempfile()))
        tex <- utils::Sweave(rnw)
        pdf <- tools::texi2pdf(tex)
    }
}
