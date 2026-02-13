
# Prostar

[![Project Status: Active - The project has reached a stable, usable state and is being actively developed.](http://www.repostatus.org/badges/latest/active.svg)](http://www.repostatus.org/#active)

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
library(Prostar2)
library(MagellanNTK)
library(DaparToolshed)
library(omXplore)

Prostar2("PipelineProtein")
```