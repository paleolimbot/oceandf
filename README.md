
<!-- README.md is generated from README.Rmd. Please edit that file -->

# oceandf

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
<!-- badges: end -->

The goal of oceandf is to read ODF files. You can already do this using
`oce::read.ctd.odf()`; this package allows slightly better header
parsing and faster/more flexible reading using the
[readr](https://readr.tidyverse.org) package.

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("remotes")
remotes::install_github("paleolimbot/oceandf")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(oceandf)
read_odf("some_file.odf")
read_odf_colmeta("some_file.odf")
```
