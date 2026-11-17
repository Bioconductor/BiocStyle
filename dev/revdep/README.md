# Reverse-dependency vignette sweep

Renders the vignettes of Bioconductor packages that use BiocStyle twice:
once with a **baseline** BiocStyle and once with a **candidate**. It then
reports what changed. Run it before merging a BiocStyle change that could
affect how other packages' vignettes render.

## Running it

From the repository root (needs Docker and [just](https://just.systems)):

```sh
# default: baseline=origin/devel, candidate=. (your working tree, uncommitted edits included), n=20
just revdep-sweep

just revdep-sweep baseline=origin/devel candidate=. n=60
just revdep-sweep candidate=../BiocStyle-other-checkout n=10
just revdep-sweep candidate=my-branch pkgs=Glimma,DESeq2   # only these packages

just revdep-sweep resume=20261117-094607     # continue an interrupted run
just revdep-report run=20261117-094607       # rebuild report.md only
just revdep-clean-cache                      # drop the cached R library volume
```

Without just, run `dev/revdep/run.sh sweep key=value ...` instead.

| option      | default              | meaning |
|-------------|----------------------|---------|
| `baseline`  | `origin/devel`       | git ref, or a directory to copy |
| `candidate` | `.`                  | git ref, or a directory to copy (the working tree by default) |
| `n`         | `20`                 | top-n reverse dependencies by download score |
| `pkgs`      |                      | comma-separated packages; replaces the top-n + forced selection |
| `forced`    | `dev/revdep/forced.txt` | extra packages always included |
| `timeout`   | `600`                | per-vignette render timeout, seconds |
| `jobs`      | `1`                  | `2` renders baseline and candidate at the same time (see caveats) |
| `platform`  | native               | e.g. `linux/amd64` to force emulation |
| `resume`    |                      | name or path of an existing run directory |

A ref is resolved with `git rev-parse` and exported with `git archive`. A
directory is copied with its `.git` and `dev/` left out. Both snapshots
are saved in the run directory, so `resume=` reuses exactly the same
sources even if the ref has moved since.

Output goes to `dev/revdep/results/<timestamp>/` (gitignored):

- `report.md`: the summary (new failures, fixed, changed output, failing
  in both, package-level problems, per-vignette table, timings)
- `sample.tsv`: the packages chosen and why
- `results.tsv`: one row per vignette, with the full error text
- `timings.tsv`: seconds per setup step and per package step
- `compare/<pkg>/`: normalized outputs and `*.diff` for anything that changed
- `pkgs/<pkg>/{baseline,candidate}/`: rendered outputs, render logs and
  status JSON; `pkgs/<pkg>/install.log` has the dependency install log

## What it does

1. `run.sh` snapshots both BiocStyles and builds `biocstyle-revdep:devel`
   from `Dockerfile`. That image is `bioconductor/bioconductor_docker:devel`
   plus poppler-utils plus TinyTeX with the packages from
   `inst/resources/tex/required-packages.txt`, installed by
   `.github/scripts/install-tex.R`, the same script CI uses.
2. `sweep.R` runs inside the container:
   - It installs each BiocStyle into its own library
     (`<run>/lib/{baseline,candidate}`). Everything else goes into a shared
     library on the named volume `biocstyle-revdep-cache-<arch>`, which
     persists between runs. BiocStyle is always removed from the shared
     library, so each render can only find its variant's copy. The report
     warns if a render loaded any other BiocStyle.
   - It selects the packages: reverse dependencies of BiocStyle in Bioc
     devel (`tools::package_dependencies(reverse = TRUE, which =
     c("Depends", "Imports", "Suggests"))`), ranked by
     `bioc_pkg_scores.tab`, top `n`, plus `forced.txt`.
   - For each package, it downloads the devel source tarball and installs
     the package with all of its dependencies, Suggests included
     (`install-pkg.R`). Then it renders every vignette (Rmd, Rnw, ...) with
     `tools::buildVignette()`, the function `R CMD build` uses. Each render
     is a fresh `Rscript` under `timeout` (`render-one.R`) with the RNG
     seeded, working in a copy of the package source. A render that fails
     on a missing LaTeX package gets the package installed through tinytex
     and is retried once.
3. `compare.R` pairs the two renders of each vignette:
   - **HTML**: normalizes both files and runs `diff -u`. Base64 `data:`
     URIs are decoded inline if they hold text (CSS/JS/SVG), so a style
     change appears as a readable line. Binary payloads (images, fonts) are
     replaced by an MD5 hash. The HTML is split to one tag per line. The
     normalization strips dates and times, `0x...` addresses, htmlwidget
     and other long hex ids, pandoc code-line anchors (`cb1-7`),
     per-variant paths, `/tmp/Rtmp*` paths, `BiocStyle_x.y.z` version
     strings, `system.time()` numbers, and AnnotationHub/ExperimentHub
     "downloading / retrieving / loading from cache" messages.
   - **PDF**: compares the page count (`pdfinfo`) and the normalized
     `pdftotext -layout` text.

## How long it takes

Measured on an Apple Silicon Mac (16 cores, Docker VM with 8 GB RAM),
native arm64:

- One-time: pulling `bioconductor_docker:devel` (7.3 GB) took 4m20s.
  Building the derived image (poppler, TinyTeX, LaTeX packages) took
  about 40 s.
- With a cold library cache, dependency installs dominate. Bioconductor
  packages compile from source on arm64; CRAN packages come as p3m
  binaries. Measured installs: S4Vectors 83 s, IRanges 39 s, Biobase
  16 s, BiocParallel 80 s, SparseArray 118 s, Glimma (scRNAseq, scater,
  scran, ...) 132 s.
- With a warm cache: top 3 + Glimma + GeneOverlap + DirichletMultinomial
  (12 vignettes) took 7m31s end to end. Glimma's `single_cell_edger.Rmd`
  accounted for about 6 minutes of that (about 175 s per render). Most
  other packages took 5–20 s for download, install check, and both
  renders.
- A sweep of `n=60` hasn't been timed yet. Expect roughly an hour or more
  with a warm cache and several hours with a cold one, depending on which
  packages make the top 60.

On an Apple Silicon Mac, the image has a native arm64 variant, which is the
default. `platform=linux/amd64` runs under emulation, which is noticeably
slower. It also uses a separate cache volume.

## Caveats

- **PDF styling changes are not detected** unless they change the text or
  the page count. A color change in `Bioconductor.sty` produced a zero
  diff during verification. HTML changes (including CSS) are detected.
- Renders run in sequence by default. `jobs=2` runs baseline and candidate
  at the same time, which is faster. However, during testing the two
  concurrent renders collided: both used the same seeded BiocParallel
  socket port, which caused spurious failures.
- The shared library is never upgraded, only added to. After a
  Bioconductor release or a new `bioconductor_docker:devel` image (new R),
  run `just revdep-clean-cache`. `docker build` does not pull a newer base
  image on its own; run `docker pull bioconductor/bioconductor_docker:devel`
  first.
- Vignettes that download data (AnnotationHub, ExperimentHub, URLs) need
  the network. The Hub cache lives on the cache volume. A remote outage
  shows up as "failing in both", not as a new failure.
- Only the Bioconductor software repository is swept. Workflows, books,
  and CRAN reverse dependencies are not included.
- Normalization is tuned on a small sample. If a sweep with identical
  baseline and candidate shows diffs, the noise rules in `compare.R` need
  extending. Run `just revdep-sweep baseline=origin/devel
  candidate=origin/devel n=...` to check.
- Glimma's devel vignettes use `rmarkdown::html_vignette`, not BiocStyle,
  so Glimma acts as a control: it should never show a change.
