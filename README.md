
<!-- README.md is generated from README.Rmd. Please edit that file -->

# oceandf

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![R build
status](https://github.com/paleolimbot/oceandf/workflows/R-CMD-check/badge.svg)](https://github.com/paleolimbot/oceandf/actions)
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
odf_file <- odf_example("CTD_98911_10P_11_DN.ODF")

read_odf(odf_file)
#> # A tibble: 562 x 6
#>    PRES      TEMP  COND  PSAL    POTM  SIGP
#>    <chr>    <dbl> <dbl> <dbl>   <dbl> <dbl>
#>  1 0.4210  0.509   2.67  31.4  0.509   25.2
#>  2 0.8000 -0.134   2.68  32.1 -0.134   25.8
#>  3 0.9890 -0.229   2.68  32.2 -0.229   25.9
#>  4 1.0840 -0.0914  2.68  32.1 -0.0914  25.8
#>  5 1.1790  0.317   2.67  31.6  0.317   25.4
#>  6 1.3680  0.222   2.67  31.7  0.222   25.4
#>  7 1.3680  0.174   2.67  31.8  0.174   25.5
#>  8 1.3680 -0.211   2.67  32.1 -0.211   25.8
#>  9 1.4630 -0.193   2.66  32.0 -0.193   25.7
#> 10 1.5580 -0.622   2.66  32.4 -0.622   26.0
#> # ... with 552 more rows
read_odf_parameter_header(odf_file)
#> # A tibble: 6 x 15
#>   index TYPE  NAME  UNITS  WMO_CODE NULL_VALUE PRINT_FIELD_WID~ PRINT_DECIMAL_P~
#>   <int> <chr> <chr> <chr>  <chr>    <chr>                 <dbl>            <dbl>
#> 1     1 SING  PRES  decib~ PRES     -.9900000~               11                4
#> 2     2 SING  TEMP  degre~ TEMP     -.9900000~               11                4
#> 3     3 SING  COND  mmHo   COND     -.9900000~               11                4
#> 4     4 SING  PSAL  <NA>   PSAL     -.9900000~               11                4
#> 5     5 SING  POTM  degre~ POTM     -.9900000~                8                4
#> 6     6 SING  SIGP  kg/m*~ SIGP     -.9900000~                8                4
#> # ... with 7 more variables: ANGLE_OF_SECTION <dbl>, MAGNETIC_VARIATION <dbl>,
#> #   DEPTH <chr>, MINIMUM_VALUE <dbl>, MAXIMUM_VALUE <dbl>, NUMBER_VALID <dbl>,
#> #   NUMBER_NULL <dbl>
names(read_odf_header(odf_file))
#> [1] "ODF_HEADER"        "CRUISE_HEADER"     "EVENT_HEADER"     
#> [4] "INSTRUMENT_HEADER" "HISTORY_HEADER"    "PARAMETER_HEADER" 
#> [7] "RECORD_HEADER"
```
