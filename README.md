
<img align="right" src = "inst/logo.png" width="135px">

mapbayr is a package for *maximum a posteriori* bayesian estimation of
individual PK/PD parameters, especially including non-linear and complex
models. Its purpose is to ease the dose individualization of anticancer
drugs in the context of therapeutic drug monitoring with:

  - on the one hand: a `mapbay_estimation` function that performs its
    estimation from an NM-TRAN like dataset and a *mapbayr model*
    object.
  - on the other hand: a suite of user-friendly **shiny** apps for
    different clinical application such as:
      - mapbayr-tki: for the re-estimation of drug concentration at a
        convenient time after dose (*i.e.* T+24h).
      - mapbayr-carbo: for dose-adjustment of carboplatin in
        [intensification
        protocol](https://doi.org/10.1158/1078-0432.ccr-17-1344).

## Requirements

mapbayr relies on [mrgsolve](https://mrgsolve.github.io/) for model
implementation and ordinary differential equation solving, which
requires [Rtools](https://cran.r-project.org/bin/windows/Rtools/) for
C++ compilation.

## *mrgsolve* model specification

*mapbayr* contains a library of *mrgsolve* model files (.cpp),
accessible with `mapbay_library`. The users are also invited to perform
map-bayesian estimation with their own mrgsolve models. These model
files should be slightly modified in order to be “loaded” in *mapbayr*
with the subsequent specifications :

### 1\. `$PREAMBLE` block

Two lines with `- drug` and `- model_ref`, describing the name of the
drug and the bibliographic reference of the model, must be filled. This
block still accepts free text to make notes about the model.

``` c
$PREAMBLE
- drug: examplinib
- model_ref: Smith et al, J Pharmacokinet, 2020
```

### 2\. `$PARAM` block

Add as many ETA as there are parameters to estimate. Refer them as ETAn
(n being the number of ETA). Set 0 as default value. Plain text provided
description will be used as internal “parameter names”. Text provided in
parentheses will be used as internal “parameter units” (use of empty
parentheses is advised for parameters without units).

``` c
$PARAM @annotated
ETA1 : 0 : CL (L/h)
ETA2 : 0 : VC (L)
ETA3 : 0 : F ()
//do not write ETA(1)
//do not write iETA
```

A `@covariates` tag must be used to record covariates in the `$PARAM`
block. Set the reference value. Plain text provided description will be
used as internal “covariate names”. Text provided in parentheses will be
used as “covariate units” (description of 0/1 coding is advised for
categorical covariates)

``` c
$PARAM @annotated @covariates
BW : 70 : Body weight (kg)
SEX : 0 : Sex (0=Male, 1=Female)
//do not write
//$PARAM @annotated
//BW : 70 : Body weight (kg)
```

When time or dose are needed as covariates, an internal routine is
embedded in mapbayr. You can refer them as TOLA and AOLA (i.e. time of
last administration, amount of last administration).

``` c
$PARAM @annotated @covariates
TOLA : 0 : Time of last adm (h)
AOLA : 100 : Amt of last adm (mg)
```

### 3\. `$OMEGA` block

Please ensure that omega values correspond to the order of the ETAs
provided in `$PARAM`. Omega values can be recorded in multiple blocks.

``` c
$OMEGA
0.123 0.456 0.789
$OMEGA @block
0.111 
0.222 0.333
```

### 4\. `$SIGMA` block

Two (diagonal) values are expected. The first will be used for the
proportional error, the second for (log) additive error.

``` c
$SIGMA 0.111 0 // proportional error 
$SIGMA 0 0.222 // (log) additive error
$SIGMA 0.333 0.444 // mixed error
```

When a parent drug and its metabolite are fitted simultaneously, four
values are expected.

``` c
//example: correlated proportional error between parent and metabolite
$SIGMA 
0.050 // proportional error on parent drug
0.000 0.000 // additive error on parent drug
0.100 0.000 0.200 // proportional error on metabolite
0.000 0.000 0.000 0.000 // additive error on metabolite
```

### 5\. `$CMT` block

A `@annotated` tag must be used to record compartments. Text provided in
parentheses will be used as internal “concentration units” (possible
values: **mg/L**, **ng/mL** or **pg/mL**). Text provided in brackets
will be used to define which parameters correspond to administration
\[ADM\] and/or observation \[OBS\] compartment.

``` c
//example: model with dual zero and first order absorption in compartment 1 & 2, respectively, and observation of parent drug + metabolite 
$CMT @annotated
DEPOT: Depot () [ADM]
CENT_PAR: examplinib central (ng/mL) [ADM, OBS]
PERIPH : examplinib peripheral
CENT_MET : methylexamplinib central (ng/mL) [OBS] 
```

### 6\. `$TABLE` block

Please refer the concentration variable to fit as `DV`. For log additive
error models, there is no need to log transform the data. Please
describe error as exponential to concentrations.

``` c
$TABLE
DV  = (CENTRAL / VC) * exp(EPS(1)) ;
```

For fitting parent drug and metabolite simultaneously, please refer to
them as PAR and MET, and define DV accordingly (DV will be used for
computation of OFV)

``` c
$TABLE
PAR = (CENT_PAR / V) * (1 + EPS(1)) ;
MET = (CENT_MET / V) * (1 + EPS(3)) ;
DV = PAR ;
if(self.cmt == 4) DV = MET ;
```

### 7\. `$MAIN` block

Double every expression containing ETA information, with ETAn (used for
estimation of parameters) and ETA(n) (generated for simulations with
random effects)

``` c
$PK
CL = TVCL * exp(ETA1 + ETA(1))
```

### 8\. `$CAPTURE` block

DV and ETAn must be captured, as well as PAR and MET for models with
parent + metabolite.

``` c
$CAPTURE 
DV ETA1 ETA2 ETA3 PAR MET
```
