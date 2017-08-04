Dear Bioconductor Developers,

we would like that in the next release most of the vignettes which use BiocStyle benefit from the updated formatting introduced over the last year. For this, all BiocStyle format functions will be switched to the new style. This transition will happen on Friday, August 18.

# BIOCSTYLE 2.0

The new style was developed in collaboration with a professional graphics designer with the aim to address some of the shortcomings of the original implementation, such as inconsistent formatting both within the document and across output formats, or non-distinctive structuring elements, for example running head and figure captions. Additionally, we introduced a bunch of new features, including:

* common formatting regardless of input (Rnw/Rmd) and output format (PDF/HTML)
* consistent layout providing a clear separation between content and structuring elements emphasized by horizontal alignment
* standardized way of specifying author affiliations
* cross references in R Markdown based on the bookdown syntax
* support for recent additions to R Markdown (floating TOC, code folding, ...)
* footnotes typeset as margin notes

The updated style is invoked through `BiocStyle::latex2`, `BiocStyle::pdf_document2` and `BiocStyle::html_document2` functions. Feel free to have a look at some live examples, for example, at http://bioconductor.org/packages/devel/bioc/vignettes/PureCN/inst/doc/PureCN.pdf (PDF from Rnw source), or http://bioconductor.org/packages/devel/bioc/vignettes/vsn/inst/doc/A-vsn.html (HTML from Rmd source).
 

# SWITCH TO THE NEW STYLE

To streamline the process, the transition will happen automatically by replacing the previous format functions by the new ones as follows. Functions whose name ends with "2": `BiocStyle::latex2`, `BiocStyle::pdf_document2` and `BiocStyle::html_document2` will be renamed by stripping the trailing "2" such that they replace their old counterparts, e.g. `BiocStyle::latex`. At the same time, the latter ones will be archived by appending "_old" to their names so that, for example, `BiocStyle:latex` will become `BiocStyle::latex_old`. Both the function names with the trailing "2" and the "_old" ones will be marked as deprecated, and made defunct after the release following the Function Deprecation Guidelines at http://bioconductor.org/developers/how-to/deprecation/. The following list summarizes the changes.

BiocStyle::latex          -> BiocStyle::latex_old
BiocStyle::latex2         -> BiocStyle::latex

BiocStyle::pdf_document   -> BiocStyle::pdf_document_old
BiocStyle::pdf_document2  -> BiocStyle::pdf_document

BiocStyle::html_document  -> BiocStyle::html_document_old
BiocStyle::html_document2 -> BiocStyle::html_document


# HOW DOES THIS AFFECT ME

## Scenario 1: Already using the new style provided by one of the functions: `latex2`, `pdf_document2` or `html_document2`

Congratulations, thank you for test driving the new style! Once these functions get deprecated, please rename your BiocStyle format function calls by removing the trailing "2".

## Scenario 2: Currently using one of the original format functions: `latex`, `pdf_document` or `html_document`

Typically, no special action will be needed. We worked hard to provide as much backwards compatibility as possible, so your vignette should compile with the new style right out of the box. However, some vignettes might require some small tweaks to build after the update. This affects mostly PDF output because of the more complex LaTeX macro definitions and the additional dependencies of the new style. Should you experience any problems, please first make sure that the source of your vignette meets the guidelines described in https://www.bioconductor.org/packages/devel/bioc/vignettes/BiocStyle/inst/doc/LatexStyle2.pdf (for .Rnw vignettes) or https://www.bioconductor.org/packages/devel/bioc/vignettes/BiocStyle/inst/doc/AuthoringRmdVignettes.html (for .Rmd vignettes). Any remaining issues should be reported at https://github.com/Bioconductor/BiocStyle/issues. As an interim solution you can still use the previous format function renamed to "*_old" in order to build your package.
