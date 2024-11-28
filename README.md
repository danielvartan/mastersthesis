# mastersthesis

<!-- quarto render --profile gfm -->
<!-- badges: start -->
[![Project Status: Active – The project has reached a stable, usable
state and is being actively
developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![OSF
DOI](https://img.shields.io/badge/DOI-10.17605/OSF.IO/YGKTS-1284C5.svg)](https://doi.org/10.17605/OSF.IO/YGKTS)
[![License:
MIT](https://img.shields.io/badge/license-MIT-green.png)](https://choosealicense.com/licenses/mit/)
[![License: CC BY
4.0](https://img.shields.io/badge/License-CC_BY_4.0-lightgrey.svg)](https://creativecommons.org/licenses/by/4.0/)
<!-- badges: end -->

## Overview

☀🌙⏰🛌💤🌍🗺️🧭📅🧬🧠🕰️📊📉📈📝

This repository contains the [Quarto
book](https://quarto.org/docs/books/) of my master’s thesis: *Is
latitude associated with chronotype?* The research compendium is
accessible via [The Open Science Framework](https://osf.io/) by clicking
[here](https://doi.org/10.17605/OSF.IO/YGKTS).

The assemble of this repository was inspired by Ben Marwick, Carl
Boettiger & Lincoln Mullen’s article [Packaging Data Analytical Work
Reproducibly Using R (and
Friends)](https://doi.org/10.1080/00031305.2017.1375986).

## How to use

All analyses in this thesis are fully reproducible and were conducted
using the [R programming language](https://www.r-project.org/) alongside
the [Quarto](https://quarto.org/) publishing system. The
[`renv`](https://rstudio.github.io/renv/) package was used to ensure
that the R environment used can be restored (see `renv.lock`). All the
computational notebooks can be found in the `qmd` directory.

It’s important to note that some restrictions apply to the availability
of the main research data, which contain personal and sensitive
information. As a result, this data cannot be publicly shared. To run
the analyses, users must have an internet connection and request a set
of access keys from the author (see the keys section below).

To reproduce the analyses do the following steps:

1.  Clone this repository.
2.  Open the R project (`mastersthesis.Rproj`).
3.  Run
    [`renv::restore()`](https://rstudio.github.io/renv//reference/restore.html)
    to install all software dependencies.
4.  Set all the keys provided by the author in the `.Renviron` file and
    the `_ssh` folder.
5.  Open and run the analysis in the computational notebook.

If you don’t feel comfortable with R, I strongly recommend checking
Hadley Wickham and Garrett Grolemund’s free and online book [R for Data
Science](https://r4ds.hadley.nz/) and the Coursera course from John
Hopkins University [Data Science: Foundations using
R](https://www.coursera.org/specializations/data-science-foundations-r).

## Keys

To access the data and run the notebooks, you will need to set the
following keys in the
[`.Renviron`](https://bookdown.org/csgillespie/efficientR/set-up.html#:~:text=2.4.6%20The%20.Renviron%20file)
file:

- `OSF_PAT`: Your OSF Personal Access Token (PAT). If you don’t have
  one, go to the settings section of your OSF account and create a new
  token.
- `MASTERSTHESIS_PASSWORD`: The password for the project’s RSA private
  key.

Additionally, you will need the following files in the project’s `_ssh`
folder:

- `id_rsa`: The project’s private RSA key.
- `id_rsa.pub`: The project’s public RSA key.

These project’s keys will be provided by the author of the thesis upon
reasonable request. As reasonable requests, I consider those made by
researchers with a legitimate interest in the thesis topic, whose
projects are of high quality, ethically conducted, and who have obtained
all necessary approvals for their research proposal.

## License

[![License:
MIT](https://img.shields.io/badge/license-MIT-green.png)](https://opensource.org/license/mit/)
[![License: CC BY
4.0](https://img.shields.io/badge/License-CC_BY_4.0-lightgrey.svg)](https://creativecommons.org/licenses/by/4.0/)

The code accompanying this thesis is distributed under the [MIT
License](https://opensource.org/license/mit/). All documents are
released under the [Creative Commons Attribution 4.0 International
Public License](https://creativecommons.org/licenses/by/4.0/).

The research data is subject to a private license and is not publicly
available due to privacy and ethical considerations.

## Citation

To cite this work, please use the following format:

Vartanian, D. (2024). *Is latitude associated with chronotype?*
\[Master’s thesis, University of São Paulo\].
<https://doi.org/10.17605/OSF.IO/YGKTS>

A BibTeX entry for LaTeX users is:

    @mastersthesis{vartanian2024,
      title = {Is latitude associated with chronotype?},
      author = {Daniel Vartanian},
      year = {2024},
      address = {São Paulo},
      school  = {University of São Paulo},
      langid = {en},
      url = {https://doi.org/10.17605/OSF.IO/YGKTS},
      note = {Original version}
    }

## Acknowledgments

This thesis was developed in the Graduate Program in Modeling Complex
Systems ([PPG-SCX](https://sites.usp.br/scx/en/apresentacao/)) at the
University of São Paulo ([USP](https://www5.usp.br/)), under the
supervision of [Prof. Dr. Camilo Rodrigues
Neto](https://orcid.org/0000-0001-6783-6695).

<a href="https://www.gov.br/capes/"><img src= "images/capes_logo_horizontal_rgb.png" height="35"/></a>

This study was financed by the Coordenação de Aperfeiçoamento de Pessoal
de Nível Superior - Brazil ([CAPES](https://www.gov.br/capes/)) -
Finance Code 001, Grant Number 88887.703720/2022-00.
