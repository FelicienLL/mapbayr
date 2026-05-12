# Check if model is valid for 'mapbayr'

Checks that the model respects points related exclusively to 'mapbayr'.
Useful at the time you wish to convert a "regular" 'mrgsolve' model you
used for simulation into a model to perform MAP-Bayesian estimation.
Note that some elements cannot be checked:

- In `$MAIN` block, make sure that you added `ETA1, ETA2...` in the
  code. For instance: `double CL = TVCL * exp(ETA(1) + ETA1) ;`.

- In `$OMEGA` block, make sure the order of the (diagonal) values is the
  same as for ETAs in `$PARAM`. For instance, if `ETA1` corresponds to
  clearance, the first value in `$OMEGA` must be the variance of
  clearance.

- In `$SIGMA` block, make sure the order is respected: proportional
  error first, and additive error secondly.

## Usage

``` r
check_mapbayr_model(x, check_compile = TRUE)
```

## Arguments

- x:

  model file

- check_compile:

  check if model is compiled

## Value

`TRUE` (invisibly) if checks are passed, errors otherwise.

## Examples

``` r
library(mapbayr)
library(mrgsolve)
if (FALSE) check_mapbayr_model(house()) # \dontrun{}
```
