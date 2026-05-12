# Estimate parameters (maximum a posteriori)

The main function of the mapbayr package. Performs a *maximum a
posteriori* Bayesian estimation of parameters, from a mrgsolve model
object and a dataset containing information about administrations and
observed concentrations.

## Usage

``` r
mapbayest(
  x,
  data = NULL,
  method = c("L-BFGS-B", "newuoa"),
  hessian = stats::optimHess,
  select_eta = NULL,
  lambda = 1,
  lloq = NULL,
  force_initial_eta = NULL,
  quantile_bound = 0.001,
  control = list(),
  check = TRUE,
  verbose = TRUE,
  progress = TRUE,
  reset = 50,
  output = NULL,
  ...
)
```

## Arguments

- x:

  the model object

- data:

  NMTRAN-like data set

- method:

  optimization method; the default is `"L-BFGS-B"` (from
  `stat::optim()`), alternatively `"newuoa"` for
  [`minqa::newuoa()`](https://rdrr.io/pkg/minqa/man/newuoa.html)

- hessian:

  function used to compute the Hessian and variance-covariance matrix
  with (default is
  [`stats::optimHess`](https://rdrr.io/r/stats/optim.html),
  alternatively use `nlmixr::nlmixrHess`)

- select_eta:

  a vector of numeric values, the numbers of the ETAs to be estimated
  (default is `NULL`, all ETAs non-equal to zero)

- lambda:

  a numeric value, the weight applied to the model prior (default is 1)

- lloq:

  a numeric value, the lower limit of quantification. If not NULL,
  `LLOQ` and `BLQ` (below limit of quantification) variables will be
  added to the data. The related records will be censored with the M3
  method. Ignored if `LLOQ` already in the data.

- force_initial_eta:

  a vector of numeric values to start the estimation from (default to 0
  for "L-BFGS-B")

- quantile_bound:

  a numeric value representing the quantile of the normal distribution
  admitted to define the bounds for L-BFGS-B (default is 0.001, i.e.
  0.1%)

- control:

  a list passed to the optimizer (see
  [`stats::optim()`](https://rdrr.io/r/stats/optim.html) or
  [`minqa::newuoa()`](https://rdrr.io/pkg/minqa/man/newuoa.html)
  documentation)

- check:

  check model code for mapbayr specification (a logical, default is
  `TRUE`)

- verbose:

  print a message whenever optimization is reset (a logical, default is
  `TRUE`)

- progress:

  print a progress bar (a logical, default is `TRUE`)

- reset:

  maximum allowed reset of the optimizer with new initial eta values if
  numerical difficulties, or with new bounds (L-BFGS-B) if estimate
  equal to a bound. (a numeric, default is 50)

- output:

  if `NULL` (the default) a mapbayests object is returned; if `df` a
  *mapbay_tab* dataframe is returned

- ...:

  for compatibility (not used)

## Value

a mapbayests object. Basically a list containing:

- model: the model object

- arg.ofv.optim, arg.ofv.fix, arg.ofv.id: arguments passed to the
  optimization function. Useful for debugging but not relevant for a
  basic usage. Access to the data with `get_data(x)`

- opt.value: the original output of the optimization function

- final_eta: a list of individual vectors of final estimates. Access it
  with `x$final_eta` or `get_eta(x)`.

- covariance: a list of individual variance-covariance matrix of
  estimation. Access it with `x$covariance` or `get_cov(x)`.

- mapbay_tab: an output table containing the results of your estimations
  (data, IPRED, PRED, covariates, captured items, ETA etc...). Access it
  with `x$mapbay_tab`, `as.data.frame(x)` or `as_tibble(x)`.

- information: run times and package versions.

## See also

[`hist.mapbayests`](https://felicienll.github.io/mapbayr/reference/hist.mapbayests.md)

[`plot.mapbayests`](https://felicienll.github.io/mapbayr/reference/plot.mapbayests.md)

[`use_posterior`](https://felicienll.github.io/mapbayr/reference/use_posterior.md)

## Examples

``` r
# First, code a model
code1 <- "$PARAM ETA1 = 0, ETA2 = 0,
KA = 0.5, TVCL = 1.1, TVV = 23.3
$OMEGA 0.41 0.32
$SIGMA 0.04 0
$CMT DEPOT CENT
$PK
double CL=TVCL*exp(ETA1+ETA(1));
double V=TVV*exp(ETA2+ETA(2)) ;
$ERROR
double DV=CENT/V*(1+EPS(1))+EPS(2);
$PKMODEL ncmt = 1, depot = TRUE
$CAPTURE DV CL
"

my_model <- mrgsolve::mcode("my_model", code1)
#> Building my_model ... 
#> done.
# Then, import your data
my_data <- data.frame(ID = 1, TIME = c(0, 1.1, 5.2, 12.3), EVID = c(1,0,0,0), AMT = c(500, 0,0,0),
 CMT = c(1,2,2,2), DV = c(0, 15.1, 29.5, 22.3))
print(my_data)
#>   ID TIME EVID AMT CMT   DV
#> 1  1  0.0    1 500   1  0.0
#> 2  1  1.1    0   0   2 15.1
#> 3  1  5.2    0   0   2 29.5
#> 4  1 12.3    0   0   2 22.3

# And estimate
my_est <- mapbayest(x = my_model, data = my_data)
print(my_est)
#> Model: my_model 
#> ID : 1 individual(s).
#> OBS: 3 observation(s).
#> ETA: 2 parameter(s) to estimate.
#> 
#> Estimates: 
#>   ID       ETA1       ETA2
#> 1  1 -0.3826972 -0.5050322
#> 
#> Output (4 lines): 
#>   ID time evid amt cmt mdv   DV IPRED  PRED   CL   ETA1   ETA2
#> 1  1  0.0    1 500   1   1  0.0   0.0  0.00 0.75 -0.383 -0.505
#> 2  1  1.1    0   0   2   0 15.1  14.6  8.83 0.75 -0.383 -0.505
#> 3  1  5.2    0   0   2   0 29.5  27.2 16.78 0.75 -0.383 -0.505
#> 4  1 12.3    0   0   2   0 22.3  20.6 13.21 0.75 -0.383 -0.505
# see also plot(my_est) and hist(my_est)

# Use your estimation
get_eta(my_est)
#>       ETA1       ETA2 
#> -0.3826972 -0.5050322 
get_param(my_est)
#> [1] 0.7502213
as.data.frame(my_est)
#>   ID time evid amt cmt mdv   DV    IPRED      PRED        CL       ETA1
#> 1  1  0.0    1 500   1   1  0.0  0.00000  0.000000 0.7502213 -0.3826972
#> 2  1  1.1    0   0   2   0 15.1 14.57114  8.825693 0.7502213 -0.3826972
#> 3  1  5.2    0   0   2   0 29.5 27.20559 16.778311 0.7502213 -0.3826972
#> 4  1 12.3    0   0   2   0 22.3 20.56632 13.208070 0.7502213 -0.3826972
#>         ETA2
#> 1 -0.5050322
#> 2 -0.5050322
#> 3 -0.5050322
#> 4 -0.5050322
use_posterior(my_est)
#> 
#> 
#> ---------------  source: my_model.cpp  ---------------
#> 
#>   project: /tmp/Rtmpky20Ox
#>   shared object: my_model-so-1b5e3f256974 
#> 
#>   time:          start: 0 end: 24 delta: 1
#>                  add: <none>
#>   compartments:  DEPOT CENT [2]
#>   parameters:    ETA1 ETA2 KA TVCL TVV [5]
#>   captures:      DV CL [2]
#>   omega:         2x2 
#>   sigma:         2x2 
#> 
#>   solver:        rtol: 1e-08 atol: 1e-08 itol: 1 (scalar)
#> ------------------------------------------------------
```
