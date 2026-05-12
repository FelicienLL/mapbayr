# Plot posterior distribution of bayesian estimates

Plot posterior distribution of bayesian estimates

## Usage

``` r
# S3 method for class 'mapbayests'
hist(x, select_eta = x$arg.optim$select_eta, shk = c("sd", "var", NA), ...)
```

## Arguments

- x:

  A `mapbayests` object.

- select_eta:

  a vector of numeric values, the numbers of the ETAs to show (default
  are estimated ETAs).

- shk:

  method to compute the shrinkage if multiple subjects are analyzed.
  Possible values are "sd" (based on the ratio of standard deviation
  like in 'NONMEM'), "var" (based on the ratio of variances like
  'Monolix'), or NA (do not show the shrinkage)

- ...:

  additional arguments (not used)

## Value

a `ggplot` object.

## Details

Use this function to plot the results of the estimations, in the form of
histograms with the *a priori* distribution in the background. For every
parameter, the inter-individual variability is displayed, as well as the
percentile of the patient in the corresponding distribution (if n = 1
patient). For additional modifications, you can add extra
`+function(...)` in order to modify the plot as a regular `ggplot2`
object.

## Examples

``` r
# \donttest{
est <- mapbayest(exmodel(ID = 1))

# Default Method
h <- hist(est)

# Can be modified with `ggplot2`
h +
  ggplot2::labs(title = "Awesome estimations")


# Select the ETAs
hist(est, select_eta = c(1,3))

# }
```
