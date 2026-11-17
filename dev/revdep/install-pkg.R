## Install one reverse dependency and all of its dependencies (Suggests
## included, since vignettes need them) into the shared cached library.
## Run as a subprocess by sweep.R so the install log is captured whole.
##
## Usage: Rscript install-pkg.R <pkg_src_dir> <tarball> <lib> <status_json>
args <- commandArgs(trailingOnly = TRUE)
stopifnot(length(args) == 4L)
src_dir <- args[[1]]
tarball <- args[[2]]
lib <- args[[3]]
status_json <- args[[4]]

options(Ncpus = parallel::detectCores(), warn = 1)
desc <- read.dcf(file.path(src_dir, "DESCRIPTION"))[1, ]
pkg <- desc[["Package"]]

dep_fields <- intersect(c("Depends", "Imports", "LinkingTo", "Suggests"), names(desc))
deps <- unlist(lapply(desc[dep_fields], function(x) {
    x <- trimws(strsplit(gsub("\\s+", " ", x), ",")[[1]])
    sub("[ (].*$", "", x[nzchar(x)])
}), use.names = FALSE)
base_pkgs <- rownames(installed.packages(priority = "base"))
## BiocStyle is deliberately never installed here: each variant provides it.
deps <- setdiff(unique(deps), c("R", base_pkgs, "BiocStyle"))

installed <- function() rownames(installed.packages(lib.loc = .libPaths()))
missing <- setdiff(deps, installed())
if (length(missing)) {
    message("Installing ", length(missing), " missing dependencies: ",
            paste(missing, collapse = ", "))
    BiocManager::install(missing, lib = lib, update = FALSE, ask = FALSE)
}

have <- tryCatch(as.character(packageVersion(pkg, lib.loc = lib)),
                 error = function(e) NA_character_)
if (!identical(have, desc[["Version"]])) {
    message("Installing ", pkg, " ", desc[["Version"]], " from ", tarball)
    install.packages(tarball, repos = NULL, type = "source", lib = lib)
}

if (dir.exists(file.path(lib, "BiocStyle"))) remove.packages("BiocStyle", lib = lib)

pkg_ok <- tryCatch(identical(as.character(packageVersion(pkg, lib.loc = lib)),
                             desc[["Version"]]), error = function(e) FALSE)
jsonlite::write_json(list(
    package_installed = pkg_ok,
    missing_dependencies = setdiff(deps, installed())
), status_json, auto_unbox = TRUE, pretty = TRUE)
if (!pkg_ok) quit(status = 1)
