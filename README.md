mapbayr
================

# mapbayr <img align="right" src = "inst/logo.png" width="135px">

mapbayr is a package for *maximum a posteriori* bayesian estimation of
individual PK/PD parameters, especially including non-linear and complex
models. Its purpose is to ease the dose individualization of anticancer
drugs in the context of therapeutic drug monitoring with:

  - one the one hand: a `mapbay_estimation` function that performs its
    estimation from an NM-TRAN like dataset and a *mapbayr model*
    object.
  - one the other hand: a suite of user-friendly **shiny** apps for
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