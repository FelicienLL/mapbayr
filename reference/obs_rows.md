# Add observation lines to a dataset

The `obs_rows()` function adds an one or several observation lines to a
dataset provided as a proper data.frame or within a 'mrgsolve' model.
Used in combination with
[`adm_rows()`](https://felicienll.github.io/mapbayr/reference/adm_rows.md)
and
[`add_covariates()`](https://felicienll.github.io/mapbayr/reference/add_covariates.md),
it helps the creation of datasets in the proper format for simulations
with 'mrgsolve' or parameter estimation with 'mapbayr', as explained in
[data_helpers](https://felicienll.github.io/mapbayr/reference/data_helpers.md).

## Usage

``` r
obs_rows(x, ...)

# S3 method for class 'data.frame'
obs_rows(
  x,
  ID = NULL,
  time = NULL,
  evid = 0L,
  cmt,
  DV = NA_real_,
  mdv = NULL,
  .datehour = NULL,
  ...
)

# S3 method for class 'missing'
obs_rows(...)

# S3 method for class 'mrgmod'
obs_rows(x, cmt = NULL, DV = NA_real_, DVmet = NULL, ...)
```

## Arguments

- x:

  either a data.frame or a 'mrgsolve' model object

- ...:

  additional columns

- ID:

  subject ID (default is 1)

- time:

  event time. Default is 0 if no previous events. Mind consistency with
  `.datehour`.

- evid:

  event identification (default is 1 for administration, 0 for
  observation)

- cmt:

  compartment (no default, except if `[OBS]` was tagged in the `$CMT`
  block in model code. See `examples`.)

- DV:

  dependent value, i.e. observed concentration.

- mdv:

  missing dependent value (default is 0 a non-missing concentration
  value to take into account for parameter estimation, 1 otherwise)

- .datehour:

  a object of class POSIXct, a number or a character vector that can be
  passed to
  [`parse_datehour()`](https://felicienll.github.io/mapbayr/reference/parse_datehour.md).
  Using `.datehour` will update the value of `time` in the dataset, with
  units in hours. Mind consistency with the `time` argument.

- DVmet:

  second observation at the same time (e.g. a metabolite, "DVmet")
  observed jointly with parent drug ("DV"). Works only if `x` is a
  'mrgsolve' model where two `[OBS]` compartments were defined (see
  `examples`)

## Value

a data.frame, or a 'mrgsolve' model with a dataset in the `@args$data`
slot (accessible with
[`get_data()`](https://felicienll.github.io/mapbayr/reference/get_x.md)).

## See also

[data_helpers](https://felicienll.github.io/mapbayr/reference/data_helpers.md)

## Examples

``` r
# Create a dataset from scratch
obs_rows(time = 12, DV = 0.12, cmt = 2)
#> # A tibble: 1 × 6
#>      ID  time  evid   cmt    DV   mdv
#>   <int> <dbl> <int> <int> <dbl> <int>
#> 1     1    12     0     2  0.12     0

# Pipe-friendly addition of observation record to a pre-existing dataset
library(magrittr)
obs_rows(time = 12, DV = 0.12, cmt = 2) %>%
  obs_rows(time = c(24, 36, 48), DV = c(0.34, 0.56, 0.78), mdv = c(0, 1, 0), cmt = 2)
#> # A tibble: 4 × 6
#>      ID  time  evid   cmt    DV   mdv
#>   <int> <dbl> <int> <int> <dbl> <int>
#> 1     1    12     0     2  0.12     0
#> 2     1    24     0     2  0.34     0
#> 3     1    36     0     2  0.56     1
#> 4     1    48     0     2  0.78     0

# Inform times using the `.datehour` argument:
obs_rows(.datehour = "2020-01-01 11:11", DV = 0.12, cmt = 1) %>%
  obs_rows(.datehour = "2020-01-02 22:22", DV = 0.34, cmt = 1) %>%
  obs_rows(time = 48, DV = 0.56, cmt = 1)
#> # A tibble: 3 × 7
#>      ID  time  evid   cmt    DV   mdv .datehour          
#>   <int> <dbl> <int> <int> <dbl> <int> <dttm>             
#> 1     1   0       0     1  0.12     0 2020-01-01 11:11:00
#> 2     1  35.2     0     1  0.34     0 2020-01-02 22:22:00
#> 3     1  48       0     1  0.56     0 2020-01-03 11:11:00

# Start from a 'mrgsolve' model
library(mrgsolve)
house() %>%
  obs_rows(time = 12, DV = 0.12, cmt = 2) %>%
  obs_rows(time = c(24, 36, 48), DV = c(0.34, 0.56, 0.78), mdv = c(0, 1, 0), cmt = 2) %>%
  mrgsim()
#> Model:  housemodel 
#> Dim:    4 x 7 
#> Time:   12 to 48 
#> ID:     1 
#>     ID time GUT CENT RESP DV CP
#> 1:   1   12   0    0   50  0  0
#> 2:   1   24   0    0   50  0  0
#> 3:   1   36   0    0   50  0  0
#> 4:   1   48   0    0   50  0  0

# Default observation compartments
# Set default observation compartments in the code with `[OBS]`
model <- mcode("model", "
$CMT @annotated
DEPOT : Depot
CENTR : Central [OBS]
", compile = FALSE)
obs_cmt(model)
#> [1] 2

# Thus, no need to manually specify `cmt = 2` anymore.
model %>%
  obs_rows(time = 12, DV = 0.12) %>%
  obs_rows(time = c(24, 36, 48), DV = c(0.34, 0.56, 0.78), mdv = c(0, 1, 0)) %>%
  get_data()
#> # A tibble: 4 × 6
#>      ID  time  evid   cmt    DV   mdv
#>   <int> <dbl> <int> <int> <dbl> <int>
#> 1     1    12     0     2  0.12     0
#> 2     1    24     0     2  0.34     0
#> 3     1    36     0     2  0.56     1
#> 4     1    48     0     2  0.78     0

# Automatic lines duplication if parent + metabolite defined in the model
model <- mcode("model", "
$CMT @annotated
DEPOT : Depot
CENTR : Central [OBS]
PERIPH : Periph
METABO : Metabo [OBS]
", compile = FALSE)
obs_cmt(model)
#> [1] 2 4

model %>%
  obs_rows(time = 12, DV = 0.12, DVmet = 120) %>%
  obs_rows(
    time = c(24, 36, 48), DV = c(0.34, 0.56, 0.78),
    mdv = c(0, 1, 0), DVmet = c(340, 560, 780)
  ) %>%
  get_data()
#> # A tibble: 8 × 6
#>      ID  time  evid   cmt     DV   mdv
#>   <int> <dbl> <int> <int>  <dbl> <int>
#> 1     1    12     0     2   0.12     0
#> 2     1    12     0     4 120        0
#> 3     1    24     0     2   0.34     0
#> 4     1    24     0     4 340        0
#> 5     1    36     0     2   0.56     1
#> 6     1    36     0     4 560        1
#> 7     1    48     0     2   0.78     0
#> 8     1    48     0     4 780        0
```
