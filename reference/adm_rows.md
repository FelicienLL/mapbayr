# Add administration lines to a dataset

The `adm_rows()` function adds an one or several administration lines to
a dataset provided as a proper data.frame or within a 'mrgsolve' model.
Used in combination with
[`obs_rows()`](https://felicienll.github.io/mapbayr/reference/obs_rows.md)
and
[`add_covariates()`](https://felicienll.github.io/mapbayr/reference/add_covariates.md),
it helps the creation of datasets in the proper format for simulations
with 'mrgsolve' or parameter estimation with 'mapbayr', as explained in
[data_helpers](https://felicienll.github.io/mapbayr/reference/data_helpers.md).

## Usage

``` r
adm_rows(x, ...)

# S3 method for class 'data.frame'
adm_rows(
  x,
  ID = NULL,
  time = NULL,
  evid = 1L,
  cmt,
  amt,
  mdv = 1L,
  addl = NULL,
  ss = NULL,
  ii = NULL,
  rate = NULL,
  .datehour = NULL,
  ...
)

# S3 method for class 'missing'
adm_rows(...)

# S3 method for class 'mrgmod'
adm_rows(x, cmt = adm_cmt(x), rate = NULL, ...)
```

## Arguments

- x:

  either a data.frame or a 'mrgsolve' model object

- ...:

  additional columns or arguments for
  [`mrgsolve::ev()`](https://mrgsolve.org/docs/reference/ev.html)

- ID:

  subject ID (default is 1)

- time:

  event time. Default is 0 if no previous events. Mind consistency with
  `.datehour`.

- evid:

  event identification (default is 1 for administration, 0 for
  observation)

- cmt:

  compartment (no default, except if `[ADM]` was tagged in the `$CMT`
  block in model code. See `examples`.)

- amt:

  dose amount (for administration records only)

- mdv:

  missing dependent value (default is 1 for administration records)

- addl:

  additional dose (optional)

- ss:

  steady-state (optional, is this dose the last of an infinity of
  administration? Yes, 1, or no, 0)

- ii:

  inter-dose interval (optional, use it with `ss` and `addl`)

- rate:

  rate of administration (optional, set to -2 if you model zero-order
  infusion. See `examples`.)

- .datehour:

  a object of class POSIXct, a number or a character vector that can be
  passed to
  [`parse_datehour()`](https://felicienll.github.io/mapbayr/reference/parse_datehour.md).
  Using `.datehour` will update the value of `time` in the dataset, with
  units in hours. Mind consistency with the `time` argument.

## Value

a data.frame, or a 'mrgsolve' model with a dataset in the `@args$data`
slot (accessible with
[`get_data()`](https://felicienll.github.io/mapbayr/reference/get_x.md)).

## See also

[data_helpers](https://felicienll.github.io/mapbayr/reference/data_helpers.md)

## Examples

``` r
# Create a dataset from scratch
adm_rows(amt = 100, cmt = 1)
#> # A tibble: 1 × 6
#>      ID  time  evid   cmt   amt   mdv
#>   <int> <dbl> <int> <int> <dbl> <int>
#> 1     1     0     1     1   100     1

# Pipe-friendly addition of administration record to a pre-existing dataset
library(magrittr)
adm_rows(amt = 100, cmt = 1) %>%
  adm_rows(time = 3, amt = 200, cmt = 1, addl = 3, ii = 1)
#> # A tibble: 2 × 8
#>      ID  time  evid   cmt   amt   mdv  addl    ii
#>   <int> <dbl> <int> <int> <dbl> <int> <dbl> <dbl>
#> 1     1     0     1     1   100     1     0     0
#> 2     1     3     1     1   200     1     3     1

# Inform times using the `.datehour` argument:
adm_rows(.datehour = "2020-01-01 11:11", amt = 100, cmt = 1) %>%
  adm_rows(.datehour = "2020-01-02 22:22", amt = 200, cmt = 1) %>%
  adm_rows(time = 48, amt = 300, cmt = 1)
#> # A tibble: 3 × 7
#>      ID  time  evid   cmt   amt   mdv .datehour          
#>   <int> <dbl> <int> <int> <dbl> <int> <dttm>             
#> 1     1   0       1     1   100     1 2020-01-01 11:11:00
#> 2     1  35.2     1     1   200     1 2020-01-02 22:22:00
#> 3     1  48       1     1   300     1 2020-01-03 11:11:00

# Start from a 'mrgsolve' model
library(mrgsolve)
house() %>%
  adm_rows(amt = 100, cmt = 1) %>%
  adm_rows(time = 3, amt = 200, cmt = 1, addl = 3, ii = 1) %>%
  mrgsim(delta = 1)
#> Model:  housemodel 
#> Dim:    123 x 7 
#> Time:   0 to 120 
#> ID:     1 
#>     ID time     GUT   CENT  RESP     DV     CP
#> 1:   1    0   0.000   0.00 50.00  0.000  0.000
#> 2:   1    0 100.000   0.00 50.00  0.000  0.000
#> 3:   1    1  30.119  67.83 41.38  3.391  3.391
#> 4:   1    2   9.072  84.95 36.38  4.248  4.248
#> 5:   1    3   2.732  86.96 35.07  4.348  4.348
#> 6:   1    3 202.732  86.96 35.07  4.348  4.348
#> 7:   1    4 261.062 220.23 27.23 11.012 11.012
#> 8:   1    5 278.630 386.57 19.61 19.329 19.329

# Default administration compartments
# Set default administration compartments in the code with `[ADM]`
model <- mcode("model", "
$CMT @annotated
DEPOT : Depot [ADM]
CENTR : Central
", compile = FALSE)
adm_cmt(model)
#> [1] 1

# Thus, no need to manually specify `cmt = 1` anymore.
model %>%
  adm_rows(amt = 100) %>%
  adm_rows(time = 3, amt = 200, addl = 3, ii = 1) %>%
  get_data()
#> # A tibble: 2 × 8
#>      ID  time  evid   cmt   amt   mdv  addl    ii
#>   <int> <dbl> <int> <int> <dbl> <int> <dbl> <dbl>
#> 1     1     0     1     1   100     1     0     0
#> 2     1     3     1     1   200     1     3     1

# Automatic lines duplication if multiple depot compartments
# Automatic `rate = -2` increment if model with 0-order absorption
model <- mcode("model", "
$PARAM DUR = 1.0
$CMT @annotated
DEPOT : Depot [ADM]
CENTR : Central [ADM]
$MAIN
D_CENTR = DUR ;
", compile = FALSE)
adm_cmt(model)
#> [1] 1 2

model %>%
  adm_rows(amt = 100) %>%
  adm_rows(time = 3, amt = 200, addl = 3, ii = 1) %>%
  get_data()
#> # A tibble: 4 × 9
#>      ID  time  evid   cmt   amt   mdv  addl    ii  rate
#>   <int> <dbl> <int> <int> <dbl> <int> <dbl> <dbl> <dbl>
#> 1     1     0     1     1   100     1     0     0     0
#> 2     1     0     1     2   100     1     0     0    -2
#> 3     1     3     1     1   200     1     3     1     0
#> 4     1     3     1     2   200     1     3     1    -2
```
