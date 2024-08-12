
<!-- README.md is generated from README.Rmd. Please edit that file -->

# mastersthesis

<!-- badges: start -->

[![Project Status: WIP – Initial development is in progress, but there
has not yet been a stable, usable release suitable for the
public.](https://www.repostatus.org/badges/latest/wip.svg)](https://www.repostatus.org/#wip)
[![OSF
DOI](https://img.shields.io/badge/DOI-10.17605/OSF.IO/YGKTS-1284C5.svg)](https://doi.org/10.17605/OSF.IO/YGKTS)
[![License:
MIT](https://img.shields.io/badge/license-MIT-green)](https://choosealicense.com/licenses/mit/)
[![License: CC BY
4.0](https://img.shields.io/badge/License-CC_BY_4.0-lightgrey.svg)](https://creativecommons.org/licenses/by/4.0/)
<!-- badges: end -->

## Overview

☀🌙⏰🛌💤🌍🗺️🧭📅🧬🧠🕰️📊📉📈📝

This repository contains the research compendium and [Quarto
book](https://quarto.org/docs/books/) of my master’s thesis: *Is
latitude associated with the regulation of circadian rhythms in
humans?*. Its aim is to facilitate research work and to improve
reproducibility.

Click [here](https://danielvartan.github.io/mastersthesis/) to access
the thesis documents.

The assemble of this repository was inspired by Ben Marwick, Carl
Boettiger & Lincoln Mullen’s article [Packaging Data Analytical Work
Reproducibly Using R (and
Friends)](https://doi.org/10.1080/00031305.2017.1375986).

## How to use

The analyses contained in this thesis are 100% reproducible. They were
made using the [R programming language](https://www.r-project.org/) and
the [Quarto](https://quarto.org/) publishing system. The
[`renv`](https://rstudio.github.io/renv/) package was used to ensure
that the R environment used can be restored (see `renv.lock`). The
computational notebooks can be found in the `qmd` directory.

It’s important to note that some restrictions apply to the availability
of the main research data, which were used under the approval of a
Research Ethics Committee (REC) linked to the [Brazilian National
Research Ethics Committee
(CONEP)](https://conselho.saude.gov.br/Web_comissoes/conep/index.html).
As a result, this data cannot be publicly shared. To run the analyses,
users must have an internet connection and request a set of access keys
from the author.

To reproduce the analyses do the following steps:

1.  Clone this repository.
2.  Open the R project (`mastersthesis.Rproj`).
3.  Run
    [`renv::restore()`](https://rstudio.github.io/renv//reference/restore.html)
    to install all software dependencies.
4.  Open and run the analysis in the computational notebook.

If you don’t feel comfortable with R, I strongly recommend checking
Hadley Wickham and Garrett Grolemund’s free and online book [R for Data
Science](https://r4ds.had.co.nz/) and the Coursera course from John
Hopkins University [Data Science: Foundations using
R](https://www.coursera.org/specializations/data-science-foundations-r)
(free for audit students).

## License

[![License:
MIT](https://img.shields.io/badge/license-MIT-green)](https://opensource.org/license/mit/)
[![License: CC BY
4.0](https://img.shields.io/badge/License-CC_BY_4.0-lightgrey.svg)](https://creativecommons.org/licenses/by/4.0/)

Code related to this repository is released under the [MIT
license](https://opensource.org/license/mit/). Documents are released
under the [Creative Commons Attribution 4.0 International
license](https://creativecommons.org/licenses/by/4.0/).

The main research data were used under the approval of a Research Ethics
Committee (REC) linked to the [Brazilian National Research Ethics
Committee
(CONEP)](https://conselho.saude.gov.br/Web_comissoes/conep/index.html);
therefore, it cannot be publicly shared. However, it can be provided by
the author upon reasonable request. If the intention is to use the data
for new research, the request will need approval from CONEP, which must
be made with the author’s approval and participation.

## Citation

You can find the thesis citation below.

    Vartanian, D. (2024). Is latitude associated with the regulation of circadian rhythms in humans? [Master's thesis, University of São Paulo]. https://github.com/danielvartan/mastersthesis

A BibTeX entry for LaTeX users is

    @mastersthesis{vartanian_2023,
      title = {Is latitude associated with the regulation of circadian rhythms in humans?},
      author = {Daniel Vartanian},
      year = {2024},
      address = {São Paulo},
      school  = {University of São Paulo},
      langid = {en-us},
      url = {https://github.com/danielvartan/mastersthesis},
      note = {Preliminary version}
    }

## Acknowledgments

This Master’s thesis was developed in the Graduate Program in Modeling
Complex Systems ([PPG-SCX](https://sites.usp.br/scx/en/apresentacao/))
at the University of São Paulo ([USP](https://www5.usp.br/)), under the
supervision of [Prof. Dr. Camilo Rodrigues
Neto](https://orcid.org/0000-0001-6783-6695).

<a href="https://www.gov.br/capes/"><img src= "images/capes_logo_horizontal_rgb.png" height="35"/></a>

This study was financed by the Coordenação de Aperfeiçoamento de Pessoal
de Nível Superior - Brazil ([CAPES](https://www.gov.br/capes/)) -
Finance Code 001, Grant Number: 88887.703720/2022-00.
