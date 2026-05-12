# Data helpers: functions to build the dataset

Use
[`adm_rows()`](https://felicienll.github.io/mapbayr/reference/adm_rows.md),
[`obs_rows()`](https://felicienll.github.io/mapbayr/reference/obs_rows.md)
and
[`add_covariates()`](https://felicienll.github.io/mapbayr/reference/add_covariates.md)
to create or modify a dataset from scratch, from a pre-existing dataset,
or from a dataset stored into a 'mrgsolve' model.

## Details

Instead of importing a '.csv' file, or painfully build a data set with a
call to [`data.frame()`](https://rdrr.io/r/base/data.frame.html) and
mind how to format the data, you can pass information about:

- administrations with
  [`adm_rows()`](https://felicienll.github.io/mapbayr/reference/adm_rows.md),

- observations with
  [`obs_rows()`](https://felicienll.github.io/mapbayr/reference/obs_rows.md),

- covariates with
  [`add_covariates()`](https://felicienll.github.io/mapbayr/reference/add_covariates.md),

all being called jointly with a pipe (`%>%` or `|>`). These functions
can be used to create or modify a dataset as a proper data.frame, or to
create or modify a dataset within a 'mrgsolve' model (`@args$data`
slot). The latter is particularly useful in order to:

- automatically use default administration and observation compartments,

- automatically duplicate rows if there are several depot compartments,

- automatically set `rate = -2` if model has zero-order absorption
  pathways,

- automatically duplicate rows if concentrations of Parent drug and
  Metabolite are observed together,

- automatically add "Amount Of Last Administration" and "Time Of Last
  Administration" variables if these are covariates,

- subsequently call
  [`mrgsim()`](https://mrgsolve.org/docs/reference/mrgsim.html) or
  [`mapbayest()`](https://felicienll.github.io/mapbayr/reference/mapbayest.md).

## Examples

``` r
library(magrittr)
# First option: work with a data.frame

adm_rows(amt = 1000, cmt = 1, addl = 4, ii = 12) %>%
  obs_rows(time = c(12.3, 45.6), DV = c(.111, .222), cmt = 2) %>%
  obs_rows(time = 48, cmt = 2) %>%
  add_covariates(BW = 90, SEX = 0, TOLA = TRUE)
#> # A tibble: 8 × 12
#>      ID  time  evid   cmt   amt     DV   mdv  addl    ii    BW   SEX  TOLA
#>   <int> <dbl> <int> <int> <dbl>  <dbl> <int> <dbl> <dbl> <dbl> <dbl> <dbl>
#> 1     1   0       1     1  1000 NA         1     0     0    90     0     0
#> 2     1  12       1     1  1000 NA         1     0     0    90     0    12
#> 3     1  12.3     0     2     0  0.111     0     0     0    90     0    12
#> 4     1  24       1     1  1000 NA         1     0     0    90     0    24
#> 5     1  36       1     1  1000 NA         1     0     0    90     0    36
#> 6     1  45.6     0     2     0  0.222     0     0     0    90     0    36
#> 7     1  48       1     1  1000 NA         1     0     0    90     0    48
#> 8     1  48       0     2     0 NA         1     0     0    90     0    48

# You can even inform "time" using date and hours:
adm_rows(amt = 1000, cmt = 1, addl = 4, ii = 12, .datehour = "2022-01-01 11:11:11") %>%
  obs_rows(.datehour = "2022-01-02 22:22:22", DV = 0.111, cmt = 2)
#> # A tibble: 2 × 10
#>      ID  time  evid   cmt   amt     DV   mdv  addl    ii .datehour          
#>   <int> <dbl> <int> <int> <dbl>  <dbl> <int> <dbl> <dbl> <dttm>             
#> 1     1   0       1     1  1000 NA         1     4    12 2022-01-01 11:11:11
#> 2     1  35.2     0     2     0  0.111     0     0     0 2022-01-02 22:22:22

# Second option: work with a dataset within a 'mrgsolve' model
# \donttest{
mod <- exmodel(add_exdata = FALSE)
# call `mrgsolve::see(mod)` to see how default compartment were coded
adm_cmt(mod)
#> [1] 1
obs_cmt(mod)
#> [1] 2

mod %>%
  adm_rows(amt = 10000) %>%
  obs_rows(time = c(1.5, 4.4, 7.5, 24.6), DV = c(91.2904, 110.826, 79.384, 20.6671)) %>%
  # get_data() # for curiosity, you can extract the data set at this step
  mapbayest()
#> Model: mrg_001 
#> ID : 1 individual(s).
#> OBS: 4 observation(s).
#> ETA: 3 parameter(s) to estimate.
#> 
#> Estimates: 
#>   ID      ETA1       ETA2        ETA3
#> 1  1 0.4001384 0.06261949 -0.08754209
#> 
#> Output (5 lines): 
#>   ID time evid cmt   amt mdv    DV IPRED  PRED ETA1   ETA2    ETA3
#> 1  1  0.0    1   1 10000   1    NA   0.0   0.0  0.4 0.0626 -0.0875
#> 2  1  1.5    0   2     0   0  91.3  93.2 105.3  0.4 0.0626 -0.0875
#> 3  1  4.4    0   2     0   0 110.8 100.8 116.0  0.4 0.0626 -0.0875
#> 4  1  7.5    0   2     0   0  79.4  80.5  98.6  0.4 0.0626 -0.0875
#> 5  1 24.6    0   2     0   0  20.7  20.5  37.2  0.4 0.0626 -0.0875
# }
```
