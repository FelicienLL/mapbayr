# Pre-process: arguments for optimization function

Pre-process: arguments for optimization function

## Usage

``` r
preprocess.optim(
  x,
  method = c("L-BFGS-B", "newuoa"),
  select_eta = NULL,
  control = list(),
  force_initial_eta = NULL,
  quantile_bound = 0.001
)
```

## Arguments

- x:

  the model object

- method:

  optimization method; the default is `"L-BFGS-B"` (from
  `stat::optim()`), alternatively `"newuoa"` for
  [`minqa::newuoa()`](https://rdrr.io/pkg/minqa/man/newuoa.html)

- select_eta:

  a vector of numeric values, the numbers of the ETAs to be estimated
  (default is `NULL`, all ETAs non-equal to zero)

- control:

  a list passed to the optimizer (see
  [`stats::optim()`](https://rdrr.io/r/stats/optim.html) or
  [`minqa::newuoa()`](https://rdrr.io/pkg/minqa/man/newuoa.html)
  documentation)

- force_initial_eta:

  a vector of numeric values to start the estimation from (default to 0
  for "L-BFGS-B")

- quantile_bound:

  a numeric value representing the quantile of the normal distribution
  admitted to define the bounds for L-BFGS-B (default is 0.001, i.e.
  0.1%)

## Value

a list of named arguments passed to optimizer (i.e. arg.optim)
