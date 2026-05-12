# Generate a vector of "ETA"

Generate a vector of "ETA" values. If x is a `mrgsolve` model, these
will be extracted from values defined in `$PARAM`. Otherwise, any
numeric values passed to `x` and `...` as vector(s) or list(s) will be
coerced as a single vector. Alternatively, if `x` and `...` are missing,
generate a vector of ETA equal to `val` of length `n`.

## Usage

``` r
eta(x, ..., n, val = 0)
```

## Arguments

- x:

  either a `mrgsolve` model object, or a numeric

- ...:

  additional numeric(s)

- n, val:

  generate a sequence of `val` of length `n`

## Value

a single named vector of numeric

## Examples

``` r
# Extract ETA from the model
mod <- exmodel()
eta(mod)
#> ETA1 ETA2 ETA3 
#>    0    0    0 

# Coerce numeric values
eta(0.1, 0.2, c(0.3, 0.4), list(0.5, 0.6))
#> ETA1 ETA2 ETA3 ETA4 ETA5 ETA6 
#>  0.1  0.2  0.3  0.4  0.5  0.6 
eta(rnorm(4))
#>        ETA1        ETA2        ETA3        ETA4 
#>  0.66082030 -0.52291238  0.68374552 -0.06082195 

# Generate a sequence from scratch
eta(n = 3)
#> ETA1 ETA2 ETA3 
#>    0    0    0 
eta(n = 3, val = 0.001)
#>  ETA1  ETA2  ETA3 
#> 0.001 0.001 0.001 
```
