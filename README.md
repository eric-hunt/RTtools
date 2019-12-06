
<!-- README.md is generated from README.Rmd. Please edit that file -->

# RTtools <img src='man/figures/logo.png' align="right" height="139" />

<!-- badges: start -->

<!-- badges: end -->

## Overview

`RTtools` is an R package for working with real-time fluorescence assay
data including useful tools for tasks such as importing and parsing
instruemnt export files, plotting spectra, and curve fitting and
modeling data for downstream numeric comparison.

  - `import_traces()` imports real-time fluorescence data
  - `view_traces()` generates plots of real-time fluorescence curves
  - `fit_model` fits a logarithmic growth curve to the fluorescence
    curves and extracts numeric values for growth rate (rise time) and
    endpoint fluorescence
  - `view_model` generates a plot of the model parameters *endpoint* vs
    *rate*

## Installation

<!--
You can install the released version of RTtools from [CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("RTtools")
```
-->

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("eric-hunt/RTtools")
```

<!--
## Example

This is a basic example which shows you how to solve a common problem:


```r
## library(RTtools)
## basic example code
```
-->
