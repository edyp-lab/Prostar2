# Prostar2

<!-- badges: start -->
[![Project Status: Active – The project has reached a stable, usable state and is being actively developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![R-CMD-check.yaml](https://github.com/edyp-lab/Prostar2/actions/workflows/check-standard.yaml/badge.svg)](https://github.com/edyp-lab/Prostar2/actions/workflows/check-standard.yaml)
[![R-CMD-check-bioc](https://github.com/edyp-lab/Prostar2/actions/workflows/check-bioc.yml/badge.svg)](https://github.com/edyp-lab/Prostar2/actions/workflows/check-bioc.yml)
[![codecov.io](https://codecov.io/github/edyp-lab/Prostar2/coverage.svg?branch=master)](https://codecov.io/github/edyp-lab/Prostar2?branch=master)
[![license](https://img.shields.io/badge/license-Artistic--2.0-brightgreen.svg)](https://opensource.org/licenses/Artistic-2.0)
[![CRAN status](https://www.r-pkg.org/badges/version/Prostar2)](https://CRAN.R-project.org/package=Prostar2)
[![Lifecycle: stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
[![R-CMD-check](https://github.com/edyp-lab/Prostar2/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/edyp-lab/Prostar2/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

Prostar2 is a package which provides all the necessary functions to analyze quantitative data from label-free proteomics experiments. 
Contrarily to most other similar R packages, it is endowed with rich and user-friendly graphical interfaces, so that no programming skill is required.


## Install Prostar2

Prostar2 is currently available only through github. 

```r
devtools::install_github("edyp-lab/Prostar2", force=TRUE)
```


## Launch Prostar2

Prostar2 provides dedicated workflows for protein-level, peptide-level, and single-cell data.

```r
Prostar2("PipelineProtein")
```