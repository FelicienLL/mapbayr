# Get content from object

Helpful functions to get content from a `mrgmod` object (i.e. data) or
from a `mapbayests` object (`data`, `eta`, `cov`, `param`, `phi`).

## Usage

``` r
get_data(x, ...)

# S3 method for class 'mrgmod'
get_data(x, ...)

# S3 method for class 'mapbayests'
get_data(x, ..., output = "df")

get_eta(x, ...)

# S3 method for class 'mapbayests'
get_eta(x, ..., output = NULL)

get_cov(x, ...)

# S3 method for class 'mapbayests'
get_cov(x, ..., simplify = TRUE)

get_param(x, ...)

# S3 method for class 'mapbayests'
get_param(x, ..., output = NULL, keep_ID = NULL, keep_names = NULL)

get_phi(x, ...)

# S3 method for class 'mapbayests'
get_phi(x, ...)
```

## Arguments

- x:

  mapbayests object

- ...:

  not used

- output:

  either a data.frame ("df") or a vector of numeric ("num"). Default to
  "num" if only one ID

- simplify:

  a logical. If TRUE (the default) and only one ID, one matrix is
  returned instead of a list of length 1

- keep_ID:

  a logical. By default, the ID variable is dropped if one ID in data.

- keep_names:

  a logical. By default, names are dropped if one parameter is
  requested, and output is not a data frame.

## Value

the class of the object returned depends on the function, and on their
arguments. Typically, a data.frame or a vector if the output can be
reduced to one line.

## Examples

``` r
# \donttest{
# From a model object (mrgmod)
mod <- exmodel(ID = 1:2, cache = FALSE, capture = "CL")
get_data(mod)
#> # A tibble: 7 × 9
#>      ID  time  evid   amt   cmt    ii  addl   mdv    DV
#>   <int> <dbl> <int> <int> <int> <int> <int> <int> <dbl>
#> 1     1   0       1 10000     1     0     0     1  NA  
#> 2     1   1.5     0     0     2     0     0     0  91.3
#> 3     1   4.4     0     0     2     0     0     0 111. 
#> 4     1   7.1     0     0     2     0     0     0  79.4
#> 5     1  24.6     0     0     2     0     0     0  20.7
#> 6     2   0       1 10000     1     0     0     1  NA  
#> 7     2  25.9     0     0     2     0     0     0  45.2

# From an estimation object (mapbayests)
est <- mapbayest(mod)
get_data(est)
#> # A tibble: 7 × 9
#>      ID  time  evid   amt   cmt    ii  addl   mdv    DV
#>   <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#> 1     1   0       1 10000     1     0     0     1  NA  
#> 2     1   1.5     0     0     2     0     0     0  91.3
#> 3     1   4.4     0     0     2     0     0     0 111. 
#> 4     1   7.1     0     0     2     0     0     0  79.4
#> 5     1  24.6     0     0     2     0     0     0  20.7
#> 6     2   0       1 10000     1     0     0     1  NA  
#> 7     2  25.9     0     0     2     0     0     0  45.2
get_data(est, output = "list")
#> $`1`
#> # A tibble: 5 × 9
#>      ID  time  evid   amt   cmt    ii  addl   mdv    DV
#>   <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#> 1     1   0       1 10000     1     0     0     1  NA  
#> 2     1   1.5     0     0     2     0     0     0  91.3
#> 3     1   4.4     0     0     2     0     0     0 111. 
#> 4     1   7.1     0     0     2     0     0     0  79.4
#> 5     1  24.6     0     0     2     0     0     0  20.7
#> 
#> $`2`
#> # A tibble: 2 × 9
#>      ID  time  evid   amt   cmt    ii  addl   mdv    DV
#>   <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#> 1     2   0       1 10000     1     0     0     1  NA  
#> 2     2  25.9     0     0     2     0     0     0  45.2
#> 

get_eta(est)
#> # A tibble: 2 × 4
#>   ID      ETA1   ETA2     ETA3
#>   <chr>  <dbl>  <dbl>    <dbl>
#> 1 1      0.405 0.0729 -0.0750 
#> 2 2     -0.145 0.0241 -0.00619
get_eta(est, output = "list")
#> $`1`
#>        ETA1        ETA2        ETA3 
#>  0.40505701  0.07285029 -0.07495339 
#> 
#> $`2`
#>         ETA1         ETA2         ETA3 
#> -0.145365840  0.024058460 -0.006188025 
#> 

get_cov(est)
#> $`1`
#>               [,1]        [,2]          [,3]
#> [1,]  0.0136824732 0.005035304 -0.0003051945
#> [2,]  0.0050353038 0.023715037  0.0213065936
#> [3,] -0.0003051945 0.021306594  0.1322138610
#> 
#> $`2`
#>              [,1]        [,2]         [,3]
#> [1,]  0.029079985 0.028338546 -0.007325907
#> [2,]  0.028338546 0.177287758  0.001263827
#> [3,] -0.007325907 0.001263827  0.201054158
#> 

get_param(est)
#>   ID       CL
#> 1  1 5.997552
#> 2  2 3.458824

get_phi(est)
#> # A tibble: 2 × 12
#>   SUBJECT_NO    ID   ETA1   ETA2     ETA3 ETC1_1  ETC2_1 ETC2_2   ETC3_1  ETC3_2
#>        <dbl> <dbl>  <dbl>  <dbl>    <dbl>  <dbl>   <dbl>  <dbl>    <dbl>   <dbl>
#> 1          1     1  0.405 0.0729 -0.0750  0.0137 0.00504 0.0237 -3.05e-4 0.0213 
#> 2          2     2 -0.145 0.0241 -0.00619 0.0291 0.0283  0.177  -7.33e-3 0.00126
#> # ℹ 2 more variables: ETC3_3 <dbl>, OBJ <dbl>
# }
```
