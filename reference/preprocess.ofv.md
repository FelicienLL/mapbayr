# Preprocess model and data for ofv computation

Functions to generate arguments passed to
[`compute_ofv`](https://felicienll.github.io/mapbayr/reference/compute_ofv.md).
Arguments that are fixed between individuals are created once
(`preprocess.ofv.fix`), while others are specific of each individual
(`preprocess.ofv.id`).

## Usage

``` r
preprocess.ofv.fix(x, data, select_eta = seq_along(eta(x)), lambda = 1)

preprocess.ofv.id(x, iddata)
```

## Arguments

- x:

  the model object

- data, iddata:

  NMTRAN-like data set. iddata is likely a dataset of one individual

- select_eta:

  numbers of the ETAs taken into account. Set the dimensions of the
  inversed OMEGA matrix

- lambda:

  a numeric value, the weight applied to the model prior (default is 1)

## Value

A list of arguments used to compute the objective function value.

The following arguments are fixed between individuals:

- `qmod`: model object, modified to simulate without random effects and
  with controlled outputs

- `sigma`: a single matrix object

- `log_transformation`: a logical, whether predictions need to be
  log-transformed for ofv computation

- `omega_inv`: a single matrix object

- `all_cmt`: a vector of compartment numbers where observations can be
  expected

The following arguments differs between individuals:

- `idvaliddata`: a matrix, individual data set (with administrations and
  covariates), validated with
  [`valid_data_set`](https://mrgsolve.org/docs/reference/valid_data_set.html)

- `idDV`: a vector of (possibly log-transformed) observations

- `idcmt`: a vector of compartments where observations belong to

- `idblq`,`idlloq`: optional, a logical and numerical vector indicating
  if the observation is below the lower limit of quantification, and the
  LLOQ value, respectively

## Examples

``` r
mod <- exmodel(add_exdata = FALSE, compile = FALSE)
dat <- exdata(ID = c(1,4))

preprocess.ofv.fix(x = mod, data = dat)
#> $qmod
#> 
#> 
#> ---------------  source: mrg_001.cpp  ---------------
#> 
#>   project: /home/runner/wor...pbayr/exmodel
#>   shared object: mrg_001-so-1a3413e7020f 
#> 
#>   time:          start: 0 end: -1 delta: 1
#>                  add: <none>
#>   compartments:  DEPOT CENTRAL [2]
#>   parameters:    TVCL TVVC TVKA [3]
#>   captures:      DV [1]
#>   omega:         3x3 
#>   sigma:         2x2 
#> 
#>   solver:        rtol: 1e-08 atol: 1e-08 itol: 1 (scalar)
#> ------------------------------------------------------
#> 
#> $sigma
#>      [,1] [,2]
#> [1,] 0.05    0
#> [2,] 0.00    0
#> 
#> $log_transformation
#> [1] FALSE
#> 
#> $omega_inv
#>      [,1] [,2] [,3]
#> [1,]    5    0    0
#> [2,]    0    5    0
#> [3,]    0    0    5
#> 
#> $all_cmt
#> [1] 2
#> 
#> $lambda
#> [1] 1
#> 
preprocess.ofv.id(x = mod, iddata = dat[dat$ID == 1,])
#> $idvaliddata
#>      ID time evid   amt cmt ii addl mdv       DV ..zeros..
#> [1,]  1  0.0    1 10000   1  0    0   1       NA         0
#> [2,]  1  1.5    0     0   2  0    0   0  91.2904         0
#> [3,]  1  4.4    0     0   2  0    0   0 110.8260         0
#> [4,]  1  7.1    0     0   2  0    0   0  79.3840         0
#> [5,]  1 24.6    0     0   2  0    0   0  20.6671         0
#> attr(,"class")
#> [1] "valid_data_set" "matrix"        
#> 
#> $idDV
#> [1]  91.2904 110.8260  79.3840  20.6671
#> 
#> $idcmt
#> [1] 2 2 2 2
#> 
preprocess.ofv.id(x = mod, iddata = dat[dat$ID == 4,])
#> $idvaliddata
#>      ID  time evid   amt cmt ii addl mdv      DV ..zeros..
#> [1,]  4   0.0    1 10000   1  0    0   1      NA         0
#> [2,]  4  72.0    1 10000   1 24    6   1      NA         0
#> [3,]  4 237.1    0     0   2  0    0   0 34.0987         0
#> attr(,"class")
#> [1] "valid_data_set" "matrix"        
#> 
#> $idDV
#> [1] 34.0987
#> 
#> $idcmt
#> [1] 2
#> 
```
