# Filter a dataset within a mrgmod

Filter a dataset within a mrgmod

## Usage

``` r
# S3 method for class 'mrgmod'
filter(.data, ..., .preserve = FALSE)
```

## Arguments

- .data:

  a mrgmod

- ..., .preserve:

  additional arguments for
  [`dplyr::filter()`](https://dplyr.tidyverse.org/reference/filter.html)

## Value

a mrgmod

## Examples

``` r
library(magrittr)
mod <- mrgsolve::mcode("mod", "$CMT FOO", compile = FALSE)
mod %>%
  adm_rows(amt = c(100, 200, 300), cmt = 1) %>%
  filter(amt != 200) %>%
  get_data()
#> # A tibble: 2 × 6
#>      ID  time  evid   cmt   amt   mdv
#>   <int> <dbl> <int> <int> <dbl> <int>
#> 1     1     0     1     1   100     1
#> 2     1     0     1     1   300     1
```
