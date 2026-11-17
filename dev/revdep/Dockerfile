## Image for the BiocStyle reverse-dependency vignette sweep.
## Bioconductor devel + poppler (pdftotext/pdfinfo) + TinyTeX with the
## LaTeX packages BiocStyle itself needs, installed by CI's
## .github/scripts/install-tex.R from inst/resources/tex/required-packages.txt
## (passed in as the "scripts" and "tex" build contexts by run.sh).
FROM bioconductor/bioconductor_docker:devel

RUN apt-get update \
    && apt-get install -y --no-install-recommends poppler-utils \
    && rm -rf /var/lib/apt/lists/*

COPY --from=scripts install-tex.R /tmp/tex-setup/install-tex.R
COPY --from=tex required-packages.txt /tmp/tex-setup/inst/resources/tex/required-packages.txt
RUN cd /tmp/tex-setup && Rscript install-tex.R && rm -rf /tmp/tex-setup
## install_tinytex() symlinks the TeX binaries into ~/bin, which is not on
## the image's PATH.
ENV PATH="/root/bin:${PATH}"
