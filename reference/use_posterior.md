# Use posterior estimation

Use posterior estimation

## Usage

``` r
use_posterior(
  x,
  update_omega = FALSE,
  update_cov = TRUE,
  update_eta = TRUE,
  .zero_re = NULL,
  simplify = TRUE
)
```

## Arguments

- x:

  A `mapbayests` object.

- update_omega:

  Update the OMEGA matrix with the variance-covariance matrix of
  estimation (a logical, default is `FALSE`).

- update_cov:

  Update the values of covariates with the individual values (a logical,
  default is `TRUE`).

- update_eta:

  Update the values of ETA with the final estimates (a logical, default
  is `TRUE`).

- .zero_re:

  Set all elements of the OMEGA or SIGMA matrix to zero. Default is
  "both" if `update_omega` is FALSE, "sigma" otherwise. (possible values
  are "both", "sigma", "omega", "none")

- simplify:

  a logical. If TRUE (the default) and only one ID, one mrgmod is
  returned instead of a list of length 1

## Value

a mrgmod, or a list of mrgmod if there is more than 1 ID

## Details

This function takes the results of an estimation (i.e. a `mapbayests`
object) and return a modified `mrgmod` in order to perform *a
posteriori* simulations. Modifications are:

- If `update_eta` is `TRUE`, the values of ETA are updated to the
  estimated values (instead of 0) in \$PARAM.

- If `update_cov` is `TRUE`, the covariates values are updated to the
  values of the individual (instead of default model values) in \$PARAM.

- If `update_omega` is `TRUE`, the values of OMEGA are updated with the
  variance-covariance matrix of estimation (i.e. an approximation of the
  *a posteriori* distribution) instead of the inter-individual
  variability (i.e. the *a priori* distribution). Use this command in
  order to derive a confidence interval of concentrations that reflects
  the uncertainty about parameter estimation when a large number of
  profiles are simulated. Note that if inter-individual variability was
  initially defined in multiple \$OMEGA blocks in the model, they will
  be collapsed to a single full matrix (this is irreversible).

- Depending on the values of `.zero_re`, the elements of \$OMEGA or
  \$SIGMA can be set to zero, whether you want to simulate one profile,
  or several in order to derive confidence/prediction intervals. It does
  not handle time-varying covariates: only the first value will be used
  as the individual value.

## Examples

``` r
library(magrittr)
est <- mapbayest(exmodel())
est %>%
  use_posterior() %>%
  mrgsolve::ev(amt = 50000) %>%
  mrgsolve::mrgsim()
#> Model:  mrg_001 
#> Dim:    26 x 5 
#> Time:   0 to 24 
#> ID:     1 
#>     ID time   DEPOT CENTRAL    DV
#> 1:   1    0     0.0       0   0.0
#> 2:   1    0 50000.0       0   0.0
#> 3:   1    1 19771.4   28880 383.6
#> 4:   1    2  7818.2   38088 505.9
#> 5:   1    3  3091.5   39688 527.1
#> 6:   1    4  1222.5   38434 510.5
#> 7:   1    5   483.4   36198 480.8
#> 8:   1    6   191.1   33705 447.7
```
