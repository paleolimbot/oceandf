
<!-- README.md is generated from README.Rmd. Please edit that file -->

# oceandf

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![R build
status](https://github.com/paleolimbot/oceandf/workflows/R-CMD-check/badge.svg)](https://github.com/paleolimbot/oceandf/actions)
<!-- badges: end -->

The goal of oceandf is to read ODF files. You can already do this using
`oce::read.odf()` and other internal Fisheries and Oceans Canada tools;
this package allows more flexible reading using the
[readr](https://readr.tidyverse.org) package and using data frames
wherever possible.

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("remotes")
remotes::install_github("paleolimbot/oceandf")
```

## Example

Read ODF files as tables, guessing column types and names from the
PARAMTER\_HEADER:

``` r
library(oceandf)
odf_file <- odf_example("CTD_98911_10P_11_DN.ODF")
read_odf(odf_file)
#> # A tibble: 562 x 6
#>     PRES    TEMP  COND  PSAL    POTM  SIGP
#>    <dbl>   <dbl> <dbl> <dbl>   <dbl> <dbl>
#>  1 0.421  0.509   2.67  31.4  0.509   25.2
#>  2 0.8   -0.134   2.68  32.1 -0.134   25.8
#>  3 0.989 -0.229   2.68  32.2 -0.229   25.9
#>  4 1.08  -0.0914  2.68  32.1 -0.0914  25.8
#>  5 1.18   0.317   2.67  31.6  0.317   25.4
#>  6 1.37   0.222   2.67  31.7  0.222   25.4
#>  7 1.37   0.174   2.67  31.8  0.174   25.5
#>  8 1.37  -0.211   2.67  32.1 -0.211   25.8
#>  9 1.46  -0.193   2.66  32.0 -0.193   25.7
#> 10 1.56  -0.622   2.66  32.4 -0.622   26.0
#> # ... with 552 more rows
```

Read ODF headers as a list of tables, guessing column types. This was
carefully designed to report parse errors rather than fail for
creatively formatted ODF files:

``` r
read_odf_header(odf_file)
#> $ODF_HEADER
#> # A tibble: 1 x 1
#>   FILE_SPECIFICATION                
#>   <chr>                             
#> 1 D1:[MSMITH]CTD_98911_10P_11_DN.ODF
#> 
#> $CRUISE_HEADER
#> # A tibble: 1 x 9
#>   COUNTRY_INSTITUTE_CODE CRUISE_NUMBER ORGANIZATION   CHIEF_SCIENTIST
#>   <chr>                  <chr>         <chr>          <chr>          
#> 1 1810                   98911         OCEAN SCIENCES JH             
#> # ... with 5 more variables: START_DATE <dttm>, END_DATE <dttm>,
#> #   PLATFORM <chr>, CRUISE_NAME <chr>, CRUISE_DESCRIPTION <chr>
#> 
#> $EVENT_HEADER
#> # A tibble: 1 x 18
#>   DATA_TYPE EVENT_NUMBER EVENT_QUALIFIER1 EVENT_QUALIFIER2 CREATION_DATE      
#>   <chr>     <chr>        <chr>            <chr>            <dttm>             
#> 1 CTD       10P          11               DN               1998-11-19 16:07:31
#> # ... with 13 more variables: ORIG_CREATION_DATE <dttm>,
#> #   START_DATE_TIME <dttm>, END_DATE_TIME <dttm>, INITIAL_LATITUDE <dbl>,
#> #   INITIAL_LONGITUDE <dbl>, END_LATITUDE <dbl>, END_LONGITUDE <dbl>,
#> #   MIN_DEPTH <dbl>, MAX_DEPTH <dbl>, SAMPLING_INTERVAL <dbl>, SOUNDING <dbl>,
#> #   DEPTH_OFF_BOTTOM <dbl>, EVENT_COMMENTS <list>
#> 
#> $INSTRUMENT_HEADER
#> # A tibble: 1 x 4
#>   INST_TYPE MODEL SERIAL_NUMBER DESCRIPTION
#>   <chr>     <chr> <chr>         <chr>      
#> 1 Sea-Bird  <NA>  253591-026    <NA>       
#> 
#> $HISTORY_HEADER
#> # A tibble: 3 x 2
#>   CREATION_DATE       PROCESS   
#>   <dttm>              <list>    
#> 1 1998-11-16 10:27:14 <chr [26]>
#> 2 1998-11-19 15:55:53 <chr [8]> 
#> 3 1998-11-19 16:07:31 <chr [15]>
#> 
#> $PARAMETER_HEADER
#> # A tibble: 6 x 14
#>   TYPE  NAME  UNITS    WMO_CODE NULL_VALUE   PRINT_FIELD_WID~ PRINT_DECIMAL_PLA~
#>   <chr> <chr> <chr>    <chr>    <chr>                   <dbl>              <dbl>
#> 1 SING  PRES  decibars PRES     -.99000000D~               11                  4
#> 2 SING  TEMP  degrees~ TEMP     -.99000000D~               11                  4
#> 3 SING  COND  mmHo     COND     -.99000000D~               11                  4
#> 4 SING  PSAL  <NA>     PSAL     -.99000000D~               11                  4
#> 5 SING  POTM  degrees~ POTM     -.99000000D~                8                  4
#> 6 SING  SIGP  kg/m**3  SIGP     -.99000000D~                8                  4
#> # ... with 7 more variables: ANGLE_OF_SECTION <dbl>, MAGNETIC_VARIATION <dbl>,
#> #   DEPTH <chr>, MINIMUM_VALUE <chr>, MAXIMUM_VALUE <chr>, NUMBER_VALID <dbl>,
#> #   NUMBER_NULL <dbl>
#> 
#> $RECORD_HEADER
#> # A tibble: 1 x 3
#>   NUM_HISTORY NUM_CYCLE NUM_PARAM
#>         <dbl>     <dbl>     <dbl>
#> 1           3       562         6
```

The header as a `list()` of `tibble()` makes it particularly well-suited
to comparing headers from many files:

``` r
library(tidyverse)

odf_files <- c(
  odf_example("CTD_98911_10P_11_DN.ODF"),
  odf_example("CTD_PRD2002-001_26_1_DN.ODF")
)

odf_files %>%
  set_names() %>% 
  map_dfr(read_odf_header_tbl, "CRUISE_HEADER", .id = "file")
#> # A tibble: 2 x 10
#>   file             COUNTRY_INSTITUT~ CRUISE_NUMBER ORGANIZATION  CHIEF_SCIENTIST
#>   <chr>            <chr>             <chr>         <chr>         <chr>          
#> 1 C:/Users/dunnin~ 1810              98911         OCEAN SCIENC~ JH             
#> 2 C:/Users/dunnin~ <NA>              PRD2002-001   BIO/Coastal ~ SP/JH          
#> # ... with 5 more variables: START_DATE <dttm>, END_DATE <dttm>,
#> #   PLATFORM <chr>, CRUISE_NAME <chr>, CRUISE_DESCRIPTION <chr>
```
