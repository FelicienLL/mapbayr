# Example model and data

A collection of example models and corresponding data to test and
explore `mapbayr`.

## Usage

``` r
exmodel(
  num = 1,
  add_exdata = TRUE,
  cache = TRUE,
  quiet = getOption("mrgsolve_mread_quiet", TRUE),
  ...,
  ID = 1,
  clean_data = TRUE
)

exdata(num = 1, ID = 1, clean_data = TRUE)
```

## Source

<https://github.com/FelicienLL/mapbayr-CPTPSP-2021>

## Arguments

- num:

  model number (see details)

- add_exdata:

  should data be automatically loaded with the model

- cache:

  read the model with
  [`mrgsolve::mread_cache()`](https://mrgsolve.org/docs/reference/mread.html)

- quiet:

  don't print messages when compiling

- ...:

  passed to
  [`mrgsolve::mread()`](https://mrgsolve.org/docs/reference/mread.html)
  or
  [`mrgsolve::mread_cache()`](https://mrgsolve.org/docs/reference/mread.html)

- ID:

  individual number to include in the data (from 1 to 8)

- clean_data:

  remove useless columns and rows from the original data

## Value

`exmodel()` reads and compiles code, and returns a (`mrgmod`) model
object. `exdata()` returns a data.frame.

## Details

Available models are:

- 1: Base model. A simple monocompartmental PK model with
  inter-individual variability on absorption constant (KA), volume of
  distribution (VC) and clearance (CL). The residual error model is
  proportional.

- 6: Complex absorption model. Dual 0- and 1st orders absorption
  phenomenons.

- 301: Time-varying covariates. A continuous covariate (body weight
  "BW") and a categorical one (sex "SEX") influence the clearance
  parameter. In the corresponding dataset, the values randomly changes
  from one record to another within a single individual.

- 401: Metabolite. The PK model of both a parent drug and its
  metabolite.

An example dataset of eight (simulated) individuals is available for
each model. Individuals differ in terms of sampling times (sparse or
rich) and dosing regimen (single or multiple dosing).

Model code and data files are stored at the location given by
`system.file("exmodel", package = "mapbayr")`.

These models and data were created for the validation study of `mapbayr`
published in [CPT:Pharmacometrics & System
Pharmacology](https://pubmed.ncbi.nlm.nih.gov/34342170/). More models
and full datasets can be accessed [in a dedicated
repository](https://github.com/FelicienLL/mapbayr-CPTPSP-2021)

## Examples

``` r
# Models can be loaded with data (the default), ready for parameter estimation
est <- mapbayest(exmodel())

# Number of subjects in dataset can be chosen up to 8 individuals
exdata(301, ID = c(5,8))
#>    ID  time evid   amt cmt ii addl mdv      DV  BW SEX
#> 43  5   0.0    1 30000   1  0    0   1      NA  93   0
#> 44  5   1.5    0     0   2  0    0   0 566.180 127   0
#> 45  5   3.0    0     0   2  0    0   0 565.750  58   1
#> 46  5   8.9    0     0   2  0    0   0 293.910 132   1
#> 47  5  20.8    0     0   2  0    0   0 194.313  74   0
#> 49  5  96.0    1 30000   1  0    0   1      NA  80   0
#> 50  5 120.0    1 30000   1  0    0   1      NA 113   1
#> 51  5 144.0    1 30000   1  0    0   1      NA 121   1
#> 52  5 168.0    1 30000   1  0    0   1      NA  63   1
#> 53  5 192.0    1 30000   1  0    0   1      NA 109   0
#> 54  5 216.0    1 30000   1  0    0   1      NA  92   1
#> 76  8   0.0    1 30000   1  0    0   1      NA  40   1
#> 77  8  72.0    1 30000   1  0    0   1      NA  72   0
#> 78  8  96.0    1 30000   1  0    0   1      NA 112   1
#> 79  8 120.0    1 30000   1  0    0   1      NA 107   1
#> 80  8 144.0    1 30000   1  0    0   1      NA  72   1
#> 81  8 168.0    1 30000   1  0    0   1      NA  68   0
#> 82  8 192.0    1 30000   1  0    0   1      NA 105   1
#> 83  8 216.0    1 30000   1  0    0   1      NA  64   0
#> 84  8 232.4    0     0   2  0    0   0 462.256  55   1
```
