# Plot predictions from mapbayests object

Plot predictions from mapbayests object

## Usage

``` r
# S3 method for class 'mapbayests'
plot(x, ..., PREDICTION = c("IPRED", "PRED"))
```

## Arguments

- x:

  A `mapbayests` object.

- ...:

  additional arguments (passed to
  [`augment.mapbayests`](https://felicienll.github.io/mapbayr/reference/augment.mapbayests.md))

- PREDICTION:

  plot either "IPRED", "PRED" or both.

## Value

a `ggplot` object.

## Details

Use this function to plot the results of the estimations, in the form of
concentration vs time profiles for every patient of the data set. For
additional modifications, you can:

- see
  [`augment.mapbayests`](https://felicienll.github.io/mapbayr/reference/augment.mapbayests.md)
  to modify the simulation output.

- add extra `+function(...)` in order to modify the plot as a regular
  `ggplot2` object.

## Examples

``` r
est <- mapbayest(exmodel(ID = 1))
plot(est, end = 48) +
  ggplot2::labs(title = "Awesome prediction")


```
