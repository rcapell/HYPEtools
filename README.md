
# HYPEtools

<!-- badges: start -->

[![R-CMD-check](https://github.com/rcapell/HYPEtools/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/rcapell/HYPEtools/actions/workflows/R-CMD-check.yaml)
[![License: LGPL v3](https://img.shields.io/badge/License-LGPL_v3-blue.svg)](https://www.gnu.org/licenses/lgpl-3.0)
<!-- badges: end -->

This repository contains source files for an R package HYPEtools, which
provides functions to work with files from and for the hydrological
model HYPE.

## Summary

HYPEtools is an add-on package to the [R software
environment](http://www.r-project.org/), providing functions to work
with the hydrological model [HYPE](http://hype.sourceforge.net/). HYPE
and HYPEtools are projects developed by the [Swedish Meteorological and
Hydrological Institute (SMHI)](http://www.smhi.se/en).

## License
HYPEtools is licensed under the [LGPL-3.0](LICENSE) license.

## Installation

You can install the development version of HYPEtools from
[GitHub](https://github.com/rcapell/HYPEtools) with:

``` r
# install.packages("devtools")
devtools::install_github("rcapell/HYPEtools") # Install Without Vignettes
devtools::install_github("rcapell/HYPEtools", build_vignettes = T) # Install With Vignettes
```

More detailed instructions on installing the provided sources or package in
R can be found [here](https://github.com/rcapell/HYPEtools/wiki/Install-and-Update-HYPEtools-from-github).

## Vignettes

Browse HYPEtools Vignettes with:

``` r
library(HYPEtools)
browseVignettes("HYPEtools")
```
