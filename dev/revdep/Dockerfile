## Image for the BiocStyle reverse-dependency vignette sweep.
## Bioconductor devel + poppler (pdftotext/pdfinfo) + TinyTeX with the
## LaTeX packages BiocStyle itself needs (shared with CI via
## .github/scripts/install-tex.R, passed in as the "scripts" build context).
FROM bioconductor/bioconductor_docker:devel

RUN apt-get update \
    && apt-get install -y --no-install-recommends poppler-utils \
    && rm -rf /var/lib/apt/lists/*

COPY --from=scripts install-tex.R /tmp/install-tex.R
RUN Rscript /tmp/install-tex.R && rm /tmp/install-tex.R
## install_tinytex() symlinks the TeX binaries into ~/bin, which is not on
## the image's PATH.
ENV PATH="/root/bin:${PATH}"
