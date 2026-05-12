# Compute full PK profile prediction from mapbayr estimates.

Compute full PK profile prediction from mapbayr estimates.

## Usage

``` r
# S3 method for class 'mapbayests'
augment(
  x,
  data = NULL,
  start = NULL,
  end = NULL,
  delta = NULL,
  ci = FALSE,
  ci_width = 90,
  ci_method = "delta",
  ci_sims = 500,
  ...
)
```

## Arguments

- x:

  A `mapbayests` object.

- data:

  dataset to pass to mrgsolve for simulation (default is dataset used
  for estimation)

- start, end, delta:

  start, end and delta of simulation time passed to
  [`mrgsim()`](https://mrgsolve.org/docs/reference/mrgsim.html) (see
  details)

- ci:

  a logical. If TRUE, compute a confidence interval around the
  prediction (default is FALSE)

- ci_width:

  a number between 0 and 100, width of the confidence interval (default
  is "90" for a 90%CI)

- ci_method:

  method to compute the confidence interval. Can be "delta" (the
  default) to use the Delta approximation. Alternatively "simulations"
  for a more accurate approach, but also more time-consuming.

- ci_sims:

  number of replicates to simulate in order to derive the confidence
  interval (default is 500)

- ...:

  additional arguments passed to
  [`mrgsim()`](https://mrgsolve.org/docs/reference/mrgsim.html)

## Value

a `mapbayests` object, augmented of an `aug_tab` data.frame.

## Details

This function is called in the background by
[`plot()`](https://rdrr.io/r/graphics/plot.default.html) in order to
simulate the full PK profile, and return a `mapbayests` object with an
additional `aug_tab` data.frame inside. The latter is used with by the
plot method. The time grid, for each PK profile (i.e. patient) is
defaulted with the minimum time in the dataset for `start` and the
maximum time in the dataset +20% for `end`. `delta` is a power of 10
(e.g. 0.1, 1, 10 etc...), automatically chosen to render visually
appealing graphs with a reasonable computing time (about 200 time
points). Additional arguments can be passed to
[`mrgsim()`](https://mrgsolve.org/docs/reference/mrgsim.html) through
`...`. Note that `recsort` is set to 3 (see mrgsolve documentation for
more details).

## Examples

``` r
#x is the result of `mapbayest()`.
#Default plot is returned by:
# plot(x)
#Argument passed to `plot()` are passed to `augment()` in the background:
# plot(x, end = 240, ci = TRUE)
#Save the augmented object if simulation time is long
# x2 <- augment(x, ci = TRUE, ci_method = "simulations", ci_sims = 10000) %>%
# plot(x2)
```
