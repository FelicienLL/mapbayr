# Add covariate columns to a dataset

The `add_covariates()` function adds an one or several covariate columns
to a dataset provided as a proper data.frame or within a 'mrgsolve'
model. Used in combination with
[`adm_rows()`](https://felicienll.github.io/mapbayr/reference/adm_rows.md)
and
[`obs_rows()`](https://felicienll.github.io/mapbayr/reference/obs_rows.md),
it helps the creation of datasets in the proper format for simulations
with 'mrgsolve' or parameter estimation with 'mapbayr', as explained in
[data_helpers](https://felicienll.github.io/mapbayr/reference/data_helpers.md).

## Usage

``` r
add_covariates(x, ...)

# S3 method for class 'data.frame'
add_covariates(x, ..., covariates = list(), AOLA = FALSE, TOLA = FALSE)

# S3 method for class 'mrgmod'
add_covariates(x, ..., covariates = list(), AOLA = NULL, TOLA = NULL)
```

## Arguments

- x:

  either a data.frame or a 'mrgsolve' model object

- ...:

  covariates values to add to the data. For each variable, supply a
  vector of length 1 or with the same number of rows. Ignored if
  `covariates` argument is used.

- covariates:

  Covariates passed as a single list of variables. Overrides `...`.

- AOLA, TOLA:

  a logical. Should the "Amount Of Last Administration" and "Time Of
  Last Administration" variables be added into the dataset? Default if
  FALSE if `x` is a dataset, TRUE if `x` is a 'mrgsolve' model where
  `AOLA` and `TOLA` are defined as covariates

## Value

a data.frame, or a 'mrgsolve' model with a dataset in the `@args$data`
slot (accessible with
[`get_data()`](https://felicienll.github.io/mapbayr/reference/get_x.md)).

## See also

[data_helpers](https://felicienll.github.io/mapbayr/reference/data_helpers.md)

## Examples

``` r
# Cannot start from scratch
if (FALSE) { # \dontrun{
add_covariates(BW = 90, SEX = 0)
} # }

library(magrittr)
adm_rows(time = c(0, 24, 48), cmt = 1, amt = c(100, 200, 300)) %>%
  add_covariates(BW = c(90, 85, 80), SEX = 0)
#> # A tibble: 3 × 8
#>      ID  time  evid   cmt   amt   mdv    BW   SEX
#>   <int> <dbl> <int> <int> <dbl> <int> <dbl> <dbl>
#> 1     1     0     1     1   100     1    90     0
#> 2     1    24     1     1   200     1    85     0
#> 3     1    48     1     1   300     1    80     0

# If covariates are stored in a list, use `covariates = `
adm_rows(time = c(0, 24, 48), cmt = 1, amt = c(100, 200, 300)) %>%
  add_covariates(covariates = list(BW = c(90, 85, 80), SEX = 0))
#> # A tibble: 3 × 8
#>      ID  time  evid   cmt   amt   mdv    BW   SEX
#>   <int> <dbl> <int> <int> <dbl> <int> <dbl> <dbl>
#> 1     1     0     1     1   100     1    90     0
#> 2     1    24     1     1   200     1    85     0
#> 3     1    48     1     1   300     1    80     0

# Missing values are filled with the "next observation carried backward" rule
adm_rows(time = c(0, 24, 48), cmt = 1, amt = c(100, 200, 300)) %>%
  add_covariates(BW = c(90, 85, 80), SEX = 0) %>%
  obs_rows(time = 36, DV = .0123, cmt = 2)
#> # A tibble: 4 × 9
#>      ID  time  evid   cmt   amt      DV   mdv    BW   SEX
#>   <int> <dbl> <int> <int> <dbl>   <dbl> <int> <dbl> <dbl>
#> 1     1     0     1     1   100 NA          1    90     0
#> 2     1    24     1     1   200 NA          1    85     0
#> 3     1    36     0     2     0  0.0123     0    80     0
#> 4     1    48     1     1   300 NA          1    80     0
# Always verify the output in case of time-varying covariates

# Possibility to add Time and Amount of last administration as covariates
adm_rows(time = c(0, 24, 48), amt = c(100, 200, 300), cmt = 1) %>%
  obs_rows(time = c(8, 16, 32, 40), cmt = 2, DV = runif(4)) %>%
  add_covariates(TOLA = TRUE, AOLA = TRUE) %>%
  obs_rows(time = 72, cmt = 2, DV = .123) # AOLA/TOLA re-updated afterwards
#> # A tibble: 8 × 9
#>      ID  time  evid   cmt   amt      DV   mdv  AOLA  TOLA
#>   <int> <dbl> <int> <int> <dbl>   <dbl> <int> <dbl> <dbl>
#> 1     1     0     1     1   100 NA          1   100     0
#> 2     1     8     0     2     0  0.0808     0   100     0
#> 3     1    16     0     2     0  0.834      0   100     0
#> 4     1    24     1     1   200 NA          1   200    24
#> 5     1    32     0     2     0  0.601      0   200    24
#> 6     1    40     0     2     0  0.157      0   200    24
#> 7     1    48     1     1   300 NA          1   300    48
#> 8     1    72     0     2     0  0.123      0   300    48

# Automatic inclusion of `TOLA`/`AOLA` if they are covariates of the model
library(mrgsolve)
#> 
#> Attaching package: ‘mrgsolve’
#> The following object is masked from ‘package:stats’:
#> 
#>     filter
model <- mcode("model", "
$PARAM @annotated @covariates
TOLA : 0 : Time Last Adm
AOLA : 0 : Amount Last Adm
", compile = FALSE)

model %>%
  adm_rows(time = c(0, 24, 48), amt = c(100, 200, 300), cmt = 1) %>%
  add_covariates() %>%
  get_data()
#> # A tibble: 3 × 8
#>      ID  time  evid   cmt   amt   mdv  AOLA  TOLA
#>   <int> <dbl> <int> <int> <dbl> <int> <dbl> <dbl>
#> 1     1     0     1     1   100     1   100     0
#> 2     1    24     1     1   200     1   200    24
#> 3     1    48     1     1   300     1   300    48
```
