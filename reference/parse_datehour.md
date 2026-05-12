# Parse value to "POSIXct"

A wrapper around functions of `lubridate`, mainly in order to transform
characters into a date-time ("POSIXct") format.

## Usage

``` r
parse_datehour(
  x,
  orders = getOption("mapbayr.datehour", default = c("Ymd HMS", "Ymd HM", "dmY HMS",
    "dmY HM"))
)
```

## Arguments

- x:

  a numeric or a character.

- orders:

  format specification for x, passed to
  [`lubridate::parse_date_time()`](https://lubridate.tidyverse.org/reference/parse_date_time.html)

## Value

a POSIXct

## Examples

``` r
# POSITct are returned as is.
parse_datehour(x = as.POSIXct("2022-02-02 22:22:22", tz = "UTC"))
#> [1] "2022-02-02 22:22:22 UTC"

# Numerics are passed to `lubridate::as_datetime()`.
parse_datehour(1643840542)
#> [1] "2022-02-02 22:22:22 UTC"

# Characters are passed to `lubridate::parse_date_time()`.
# The format used will be the one defined in `orders`
parse_datehour(x = "2022-02-02 22:22:22", orders = "Ymd HMS")
#> [1] "2022-02-02 22:22:22 UTC"
parse_datehour(x = "02-02-2022 22:22", orders = "dmY HM")
#> [1] "2022-02-02 22:22:00 UTC"

# By default, the following formats will be subsequently tried:
# "Ymd HMS", "Ymd HM", "dmY HMS", "dmY HM"

# Alternatively, set a format through `options(mapbayr.datehour)`.
# Convenient for the use `.datehour` in  `adm_rows()` and `obs_rows()`.

# Following format will return NA:
adm_rows(.datehour = "22:22 02-02-2022", amt = 100, cmt = 1)
#> # A tibble: 1 × 7
#>      ID  time  evid   cmt   amt   mdv .datehour
#>   <int> <dbl> <int> <int> <dbl> <int> <dttm>   
#> 1     1    NA     1     1   100     1 NA       

options(mapbayr.datehour = "HM dmY")
adm_rows(.datehour = "22:22 02-02-2022", amt = 100, cmt = 1)
#> # A tibble: 1 × 7
#>      ID  time  evid   cmt   amt   mdv .datehour          
#>   <int> <dbl> <int> <int> <dbl> <int> <dttm>             
#> 1     1     0     1     1   100     1 2022-02-02 22:22:00
options(mapbayr.datehour = NULL)
```
