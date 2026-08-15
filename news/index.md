# Changelog

## mapbayr 0.10.2

CRAN release: 2026-01-31

- Internal `are_comparable()` now better checks attributes between the
  objects. This avoids a conflict with the upload of dplyr 1.2.0 to
  CRAN.

## mapbayr 0.10.1

CRAN release: 2025-08-21

Fixes tests to allow new versions of mrgsolve to CRAN
([\#223](https://github.com/FelicienLL/mapbayr/issues/223)):

- Do not use `est001` from data/ in tests, since the latter always
  contains a deprecated stale mrgsolve model in it.
- Stop creating an `ORIGID` column in the code of
  [`mapbayr_vpc()`](https://felicienll.github.io/mapbayr/reference/mapbayr_vpc.md)
  to prevent unnecessary
  [`mrgsolve::update()`](https://mrgsolve.org/docs/reference/update.html)
  warnings.
- Fix a test where “lambda” was not expected in “arg.ofv.fix”
- `est001` was regenerated using mrgsolve 1.6.1.

## mapbayr 0.10.0

CRAN release: 2023-07-17

### New features

#### Model averaging

- New
  [`model_averaging()`](https://felicienll.github.io/mapbayr/reference/model_averaging.md)
  to make averaged predictions over estimations performed from several
  models. Also exports
  [`do_model_averaging()`](https://felicienll.github.io/mapbayr/reference/model_averaging.md)
  and
  [`compute_weights()`](https://felicienll.github.io/mapbayr/reference/model_averaging.md)
  for low-level implementations.

#### Prediction-corrected Visual Predictive Checks

- New
  [`mapbayr_vpc()`](https://felicienll.github.io/mapbayr/reference/mapbayr_vpc.md)
  to make prediction-corrected visual predictive checks (`pcvpc`) from a
  given model and dataset. Control the independent variable (`idv`), and
  the stratification on a numeric variable in the dataset
  (`stratify_on`).

### Minor changes and Bug Fixes

- [`hist()`](https://rdrr.io/r/graphics/hist.html) method
  ([`hist.mapbayests()`](https://felicienll.github.io/mapbayr/reference/hist.mapbayests.md))
  now shows the values of eta-shrinkage in multiple subjects setting.
  New argument `shk` to control the definition of shrinkage, either
  based on the standard deviation (`"sd"`) or on the variance (`"var"`)
  ([@LauraMvn](https://github.com/LauraMvn),
  [\#192](https://github.com/FelicienLL/mapbayr/issues/192)).
- New
  [`mapbayr_plot()`](https://felicienll.github.io/mapbayr/reference/mapbayr_plot.md)
  in order to plot results from tables (data.frame). This is the
  function now called by
  [`plot.mapbayests()`](https://felicienll.github.io/mapbayr/reference/plot.mapbayests.md)
  internally. Can plot the results of multiple estimation object
  (informed in the column “MODEL”), useful when model averaging is
  performed. Argument `MODEL_color` to force the color of a model on the
  plot.
- New `do_mapbayr_sims()` as an engine to simulate from estimation
  results. Experimental. Now mostly useful for internal or programmatic
  uses, but might be extended in the future.
- Refactor
  [`augment.mapbayests()`](https://felicienll.github.io/mapbayr/reference/augment.mapbayests.md).
  Now easier to debug and much more faster, especially when uncertainty
  on predictions is required.
- Refactor OFV computation: parameters (“ETA”) are now passed through
  the data set and not through `$PARAM`, however the definition on “ETA”
  in `$PARAM` remains mandatory.
- Now postprocesses datasets with \>=2 missing covariates
  ([\#185](https://github.com/FelicienLL/mapbayr/issues/185)).
- “ETA” parameters cannot be longer declared as “@covariates” in
  `$PARAM` to avoid hazardous behaviours
  ([@jbwoillard](https://github.com/jbwoillard),
  [\#187](https://github.com/FelicienLL/mapbayr/issues/187)).
- By default,
  [`augment()`](https://felicienll.github.io/mapbayr/reference/augment.md)
  now simulates at least 200 points per individual. Fix a bug where
  delta was miscalculated and strange-looking plots were sometimes
  generated ([@LauraMvn](https://github.com/LauraMvn),
  [\#191](https://github.com/FelicienLL/mapbayr/issues/191)).
- The `mapbay_tab` now has the same number of rows as original data
  especially if it did not have observation rows
  ([@LauraMvn](https://github.com/LauraMvn),
  [\#193](https://github.com/FelicienLL/mapbayr/issues/193)).
- With data helpers, the `.datehour` column is updated after
  `realize_addl` is being called
  ([@LauraMvn](https://github.com/LauraMvn),
  [\#194](https://github.com/FelicienLL/mapbayr/issues/194)).
- Dependencies: mrgsolve (\>= 1.0.8) to benefit from the `etasrc`
  specification.
- Suggestions: scales.
- Add Laura Morvan [@LauraMvn](https://github.com/LauraMvn) as
  contributor.

## mapbayr 0.9.0

CRAN release: 2023-02-02

### New features

#### Data Helpers

- New
  [`adm_rows()`](https://felicienll.github.io/mapbayr/reference/adm_rows.md)
  and
  [`obs_rows()`](https://felicienll.github.io/mapbayr/reference/obs_rows.md)
  replace and improve
  [`adm_lines()`](https://felicienll.github.io/mapbayr/reference/deprecations.md)
  and
  [`obs_lines()`](https://felicienll.github.io/mapbayr/reference/deprecations.md),
  respectively. See
  [`?data_helpers`](https://felicienll.github.io/mapbayr/reference/data_helpers.md)
  for a comprehensive documentation
  ([\#175](https://github.com/FelicienLL/mapbayr/issues/175)).  

- In
  [`adm_rows()`](https://felicienll.github.io/mapbayr/reference/adm_rows.md),
  [`obs_rows()`](https://felicienll.github.io/mapbayr/reference/obs_rows.md),
  and
  [`add_covariates()`](https://felicienll.github.io/mapbayr/reference/add_covariates.md),
  the first argument `x`:

  - can be missing, which enables the creation of a new dataset from
    scratch.

  - accepts a data.frame, which enables the modification of a
    pre-existing dataset
    ([\#155](https://github.com/FelicienLL/mapbayr/issues/155)). For
    example:

    ``` r

    adm_rows(amt = 100, cmt = 1) %>% 
      obs_rows(time = 24, cmt = 2, DV = 0.123)
    ```

  - still accepts an ‘mrgsolve’ model, which enables the modification of
    a dataset stored in the model arguments.

- In
  [`adm_rows()`](https://felicienll.github.io/mapbayr/reference/adm_rows.md)
  and
  [`obs_rows()`](https://felicienll.github.io/mapbayr/reference/obs_rows.md),
  new argument `.datehour` in order to compute `time` as function of
  date and hours provided as character. The value passed to `.datehour`
  is parsed with
  [`parse_datehour()`](https://felicienll.github.io/mapbayr/reference/parse_datehour.md)
  into a date-time value (“POSIXct”). For example:

  ``` r

  obs_rows(.datehour = c("2023/02/01 12:00", "2023/02/01 12:34"), DV = c(0.123, 0.456), cmt = 1)
  ```

- Data helpers now always rearrange data to fulfill the NM-TRAN
  compatibility and readability: filling missing covariate values with
  the last observation carried forward rule, relocation of NM-TRAN
  variables in the first positions etc…

#### Below Limit of Quantification

- Data below the limit of quantification can now be handled with the
  so-called “M3 method” which consists in computing the likelihood of
  being below the limit of quantification. This is achieved when the
  variables `LLOQ` (lower limit of quantification, e.g. 0.22 mg/L) and
  `BLQ` (below limit of quantification, e.g. 1 or 0) are in the data
  ([@pchelle](https://github.com/pchelle),
  [\#182](https://github.com/FelicienLL/mapbayr/issues/182)).
- This can be achieved by:
  - adding the variables `LLOQ` and `BLQ` to the data by yourself.
  - adding the variable `LLOQ` to the data by yourself: `BLQ` will
    automatically be inferred from `LLOQ` and `DV`.
  - using `mapbayest(lloq = )` to automatically add the `LLOQ` and `BLQ`
    variables in the data.
- In
  [`mapbayest()`](https://felicienll.github.io/mapbayr/reference/mapbayest.md),
  new argument `lloq` in order to add a variable `LLOQ` to the data. For
  example: `mapbayest(model, data, lloq = 0.22)`.

#### Estimation features

- In
  [`mapbayest()`](https://felicienll.github.io/mapbayr/reference/mapbayest.md),
  new argument `select_eta` in order to select the numbers of the ETAs
  to estimate. Default are ETAs related to an OMEGA not equal to zero.
  Non-selected ETAs will not be estimated and returned equal to zero.
  This can be useful in order to ignore the estimation of ETAs not of
  interest, e.g. in case of inter-occasion variability or
  non-identifiability. For example:
  `mapbayest(model, data, select_eta = c(1,3))`
  ([\#170](https://github.com/FelicienLL/mapbayr/issues/170)).

- In
  [`mapbayest()`](https://felicienll.github.io/mapbayr/reference/mapbayest.md),
  new argument `lambda` in order to modify the weight of the priors in
  the Bayesian estimation. This could be useful in order to flatten the
  priors with the objective to favor observed data instead of *a priori*
  information. For example: `mapbayest(model, data, lambda = 0.1)` to
  decrease the weight of priors of a ten-fold
  ([\#174](https://github.com/FelicienLL/mapbayr/issues/174)).

### Minor changes and bug fixes

- Export
  [`adm_rows.data.frame()`](https://felicienll.github.io/mapbayr/reference/adm_rows.md),
  [`adm_rows.missing()`](https://felicienll.github.io/mapbayr/reference/adm_rows.md),
  [`adm_rows.mrgmod()`](https://felicienll.github.io/mapbayr/reference/adm_rows.md),
  [`obs_rows.data.frame()`](https://felicienll.github.io/mapbayr/reference/obs_rows.md),
  [`obs_rows.missing()`](https://felicienll.github.io/mapbayr/reference/obs_rows.md),
  [`obs_rows.mrgmod()`](https://felicienll.github.io/mapbayr/reference/obs_rows.md),
  and
  [`add_covariates.data.frame()`](https://felicienll.github.io/mapbayr/reference/add_covariates.md),
  as new methods for data helpers.
- Export
  [`parse_datehour()`](https://felicienll.github.io/mapbayr/reference/parse_datehour.md),
  used to parse arguments passed to `.datehour` in
  [`adm_rows()`](https://felicienll.github.io/mapbayr/reference/adm_rows.md)
  and
  [`obs_rows()`](https://felicienll.github.io/mapbayr/reference/obs_rows.md).
- Export
  [`filter.mrgmod()`](https://felicienll.github.io/mapbayr/reference/filter.mrgmod.md),
  a method, wrapper around
  [`dplyr::filter()`](https://dplyr.tidyverse.org/reference/filter.html)
  for dataset stored in mrgsolve model object (‘mrgmod’).
- Deprecate
  [`adm_lines()`](https://felicienll.github.io/mapbayr/reference/deprecations.md)
  and
  [`obs_lines()`](https://felicienll.github.io/mapbayr/reference/deprecations.md).
  Stop exporting `adm_lines.mrgmod()` and `obs_lines.mrgmod()`.
- Stop exporting `see_data()`. Was deprecated since 0.4. Use
  [`get_data()`](https://felicienll.github.io/mapbayr/reference/get_x.md)
  instead.
- Suggests `lubridate`.
- In the final estimation object, new `arg.optim$select_eta` element,
  `arg.ofv.fix$omega_inv` now has the dimensions of the number of ETAs
  selected.
- OMEGA values equal to zero are allowed in the model and will be
  ignored during the estimations steps thanks to the new `select_eta`
  argument. This condition is not tested anymore with
  [`check_mapbayr_model()`](https://felicienll.github.io/mapbayr/reference/check_mapbayr_model.md).
- [`print.mapbayests()`](https://felicienll.github.io/mapbayr/reference/print.mapbayests.md),
  now only shows the estimated ETAs.
- In [`hist()`](https://rdrr.io/r/graphics/hist.html), new argument
  `select_eta` in order to select the ETAs to plot. Default are ETAs
  estimated with
  [`mapbayest()`](https://felicienll.github.io/mapbayr/reference/mapbayest.md)
  ([\#167](https://github.com/FelicienLL/mapbayr/issues/167)).
- [`eta()`](https://felicienll.github.io/mapbayr/reference/eta.md)
  properly sorts vectors of length superior to 9
  ([\#159](https://github.com/FelicienLL/mapbayr/issues/159)).
- [`use_posterior()`](https://felicienll.github.io/mapbayr/reference/deprecations.md)
  works if covariates had not been defined in data
  ([\#160](https://github.com/FelicienLL/mapbayr/issues/160)).
- In [`hist()`](https://rdrr.io/r/graphics/hist.html),
  [`get_phi()`](https://felicienll.github.io/mapbayr/reference/get_x.md)
  and
  [`plot_phi()`](https://felicienll.github.io/mapbayr/reference/vs_nonmem.md),
  ETAs are now properly re-ordered if they are more than 9
  ([\#165](https://github.com/FelicienLL/mapbayr/issues/165)).
- Classification of absolute difference equal to zero now works
  ([\#166](https://github.com/FelicienLL/mapbayr/issues/166)).
- Observations at time = 0 are now allowed
  ([\#168](https://github.com/FelicienLL/mapbayr/issues/168)).
- Fix deprecations related to `tidyverse` packages
  ([\#171](https://github.com/FelicienLL/mapbayr/issues/171)).
- Depends on `ggplot2 >= 3.4.0`.
- [`add_covariates()`](https://felicienll.github.io/mapbayr/reference/add_covariates.md)
  accepts empty arguments.
- In
  [`mapbayest()`](https://felicienll.github.io/mapbayr/reference/mapbayest.md),
  new argument `...` in order to fix compatibility issues, not used yet.

## mapbayr 0.8.0

CRAN release: 2022-09-29

### New features

- New
  [`summarise_phi()`](https://felicienll.github.io/mapbayr/reference/vs_nonmem.md)
  and
  [`bar_phi()`](https://felicienll.github.io/mapbayr/reference/vs_nonmem.md)
  summarizes the comparison of estimation of ‘mapbayr’ and ‘NONMEM’
  (i.e. classifies it as Excellent/Acceptable/Discordant) and
  graphically represents it as a bar plot.
- New [`eta()`](https://felicienll.github.io/mapbayr/reference/eta.md)
  generates numerical values named `ETA1, ETA2, ETA3...`, either from
  scratch, from a pre-existing vector or from a ‘mrgsolve’ model object.
- In [`plot()`](https://mrgsolve.org/docs/reference/plot_mrgsims.html),
  `PREDICTION = c("IPRED", "PRED")` controls to plot either “PRED”,
  “IPRED” or both
  ([\#113](https://github.com/FelicienLL/mapbayr/issues/113)).
- In
  [`add_covariates()`](https://felicienll.github.io/mapbayr/reference/add_covariates.md),
  `covariates` is relocated in last position, in the favor of `...`
  which now accepts covariate values. Calling
  `add_covariates(list(BW = 90))` will still works (with a warning) for
  the sake of compatibility but will be deprecated. Instead, just use
  `add_covariates(BW = 90)` or explicitly call
  `add_covariates(covariates = list(BW = 90))` if you want to pass
  covariate values as a list
  ([\#156](https://github.com/FelicienLL/mapbayr/issues/156)).
- In
  [`get_eta()`](https://felicienll.github.io/mapbayr/reference/get_x.md),
  `output = "num"` returns a matrix if multiple IDs are available
  instead of an error message
  ([\#145](https://github.com/FelicienLL/mapbayr/issues/145)).

### Minor changes

- Stop exporting `postprocess.optim()` and `postprocess.output()`.
  Removed due to refactoring of internal post-processing.
- Stop exporting `adm_0_cmt()`.
- In
  [`mapbayest()`](https://felicienll.github.io/mapbayr/reference/mapbayest.md),
  `reset` is now a numeric and drives the maximum allowed reset during
  optimization.
- The progress bar is now forced to appear, especially in the RStudio
  job launcher.
- [`check_mapbayr_model()`](https://felicienll.github.io/mapbayr/reference/check_mapbayr_model.md)
  now returns an error if a check fails instead of a table that
  summarized the errors.
- [`check_mapbayr_model()`](https://felicienll.github.io/mapbayr/reference/check_mapbayr_model.md)
  now only checks critical points and not suggested features.
- [`check_mapbayr_model()`](https://felicienll.github.io/mapbayr/reference/check_mapbayr_model.md)
  now explicitly forbids `IPRED`, `PRED` and `ETA1, ETA2...`
  ([\#148](https://github.com/FelicienLL/mapbayr/issues/148)).

### Internal

- Data splitting is simpler
  ([\#127](https://github.com/FelicienLL/mapbayr/issues/127)).
- Post-processing is faster and its content depends on
  `mapbayest(output = )`
  ([\#134](https://github.com/FelicienLL/mapbayr/issues/134)).
- Optimization is faster thanks to
  [`stats::optim()`](https://rdrr.io/r/stats/optim.html) if method is
  ‘L-BFGS-B’ and
  [`minqa::newuoa()`](https://rdrr.io/pkg/minqa/man/newuoa.html) if
  method is ‘newuoa’. These replace `optimx::optimx()`
  ([\#136](https://github.com/FelicienLL/mapbayr/issues/136)).
- Remove dependency to `optimx` package.
- Downgrade `tibble` package from dependency to suggestion.
- Test refactor and more tests for internal \*\_cmt functions.
- Remove unexported functions from documentation.

### Bug fixes

- [`vs_nonmem()`](https://felicienll.github.io/mapbayr/reference/vs_nonmem.md)
  and
  [`get_phi()`](https://felicienll.github.io/mapbayr/reference/get_x.md)
  works even if covariance was missing/failing in mapbayests object
  ([\#126](https://github.com/FelicienLL/mapbayr/issues/126)).
- `pred()` does not generate `NaN` if small negative concentrations were
  predicted after log-transformation
  ([\#140](https://github.com/FelicienLL/mapbayr/issues/140)).
- `pred()` does not propagate ‘mrgsolve’ error when lag time is longer
  than inter-dose interval at steady-state
  ([\#142](https://github.com/FelicienLL/mapbayr/issues/142)).
- Non-loaded shared object are now explicitly detected
  ([\#130](https://github.com/FelicienLL/mapbayr/issues/130)).
- The absence of `NA` values in `DV` if `mdv == 0` is checked
  ([\#131](https://github.com/FelicienLL/mapbayr/issues/131)).
- The compartment numbers in the data is compared to compartments
  defined in the model
  ([\#132](https://github.com/FelicienLL/mapbayr/issues/132)).
- [`check_mapbayr_model()`](https://felicienll.github.io/mapbayr/reference/check_mapbayr_model.md)
  is now called before any use the model inside
  [`mapbayest()`](https://felicienll.github.io/mapbayr/reference/mapbayest.md)
  ([\#149](https://github.com/FelicienLL/mapbayr/issues/149)).
- A better error message is rendered if covariates are not properly
  tagged in the model
  ([\#92](https://github.com/FelicienLL/mapbayr/issues/92)).
- It is possible to use sigma labels to define `DV` if error is
  exponential
  ([\#150](https://github.com/FelicienLL/mapbayr/issues/150)).
- In
  [`obs_lines()`](https://felicienll.github.io/mapbayr/reference/deprecations.md),
  `mdv` will be 1 if `DV` is set to `NA`
  ([\#147](https://github.com/FelicienLL/mapbayr/issues/147)).

## mapbayr 0.7.3

CRAN release: 2022-05-26

- Minor changes in DESCRIPTION file (CRAN requirements)

## mapbayr 0.7.2

- Change contact address in description (CRAN requirements)

## mapbayr 0.7.1

- Fix additional CRAN checks (Mac M1)

## mapbayr 0.7.0

CRAN release: 2022-05-20

### Breaking changes

- Change the outputs of pre-processing functions. For fixed elements,
  `qmod`, `omega_inv`and `all_cmt` now replace `mrgsolve_model`,
  `omega.inv` and `obs_cmt`. For individual-related elements, `idDV`
  replaces `DVobs`, `data` is removed, `idvaliddata` and `idcmt` are
  added. This can have an impact for the user since these elements are
  reported in the standard output. However, it does not change the
  behaviour of
  [`get_data()`](https://felicienll.github.io/mapbayr/reference/get_x.md).
- Change argument behaviour: `mapbayest(verbose = TRUE)` now only
  displays the messages related to optimization reset, and not the
  progression of ID being optimized which is now controlled by
  `mapbayest(progress = TRUE)`.
- Stop exporting `derivatives()`, now replaced by `mapbayr:::h()`.
- Stop exporting `mbrlib()` and associated models. See the “Model
  examples” section below.

### Model examples

The example models system was totally re-thought around a new function:
[`exmodel()`](https://felicienll.github.io/mapbayr/reference/exmodel_exdata.md).
It now embeds several models that were used in the validation study,
with a small corresponding dataset that can be loaded automatically (the
default). They are used in multiple places inside the package,
especially in tests and examples. More models could be added in the
future.

- Export
  [`exmodel()`](https://felicienll.github.io/mapbayr/reference/exmodel_exdata.md).
  See the list of available models in the documentation.  
- Export
  [`exdata()`](https://felicienll.github.io/mapbayr/reference/exmodel_exdata.md),
  to load data only.

### Miscellaneous

- New argument: `mapbayest(progress = TRUE)` displays a progress bar
  with the number of the ID being optimized.
  [\#118](https://github.com/FelicienLL/mapbayr/issues/118)
  [\#28](https://github.com/FelicienLL/mapbayr/issues/28)
- New argument value: `mapbayest(output = "eta")` returns only estimated
  ETA in order to skip most of post-processing steps.
  [\#106](https://github.com/FelicienLL/mapbayr/issues/106)
- `eta_descr()` now always returns a non-NA value even if description is
  missing. [\#87](https://github.com/FelicienLL/mapbayr/issues/87)
- New function:
  [`do_compute_ofv()`](https://felicienll.github.io/mapbayr/reference/compute_ofv.md),
  a wrapper around `do.call(compute_ofv, ...)`.
- Add Dependency: [progress](https://github.com/r-lib/progress#readme).
- Add Suggestion: [testthat](https://testthat.r-lib.org),
  [minqa](http://optimizer.r-forge.r-project.org).
  [\#120](https://github.com/FelicienLL/mapbayr/issues/120)
- Improve the performance of objective function value calculation. Now
  use a faster parameter update, pre-validate data and refactor the
  computation of the H matrix. Thanks
  [@kylebaron](https://github.com/kylebaron) for the useful suggestions.
  [\#104](https://github.com/FelicienLL/mapbayr/issues/104)
  [\#111](https://github.com/FelicienLL/mapbayr/issues/111)
- Improve tests. Now work with example models which is overall lighter,
  faster, more consistent, more unitary. Also, they are run during
  `R CMD check`.
- Improve documentation: some monographs were merged, and some gained an
  example section thanks to the new example models system.

### Bug fixes

- [`plot_phi()`](https://felicienll.github.io/mapbayr/reference/vs_nonmem.md)
  now plots correct values on the x-axis.
  [\#108](https://github.com/FelicienLL/mapbayr/issues/108)
- No warning when plotting data with DV being `NA`.
  [\#114](https://github.com/FelicienLL/mapbayr/issues/114)
- No warning when updating a model without covariates.
  [\#115](https://github.com/FelicienLL/mapbayr/issues/115)
- No systematic reset if one “ETA” to estimate.
  [\#116](https://github.com/FelicienLL/mapbayr/issues/116)
- No errors at the end of reset-related messages.
  [\#119](https://github.com/FelicienLL/mapbayr/issues/119)

## mapbayr 0.6.0

CRAN release: 2022-02-18

This version of mapbayr introduces several features that aim to express
uncertainty around the point estimate. Please note that the results of
these functions were not validated *vs* a gold-standard software such as
NONMEM. This is why they are referred as “experimental features” in the
following subsections. They are exported with the objective to ease
their future validation, and to provide a very rough idea of the
estimation uncertainty.

### Breaking changes

- Remove `data` slot in estimation object. Use
  [`get_data()`](https://felicienll.github.io/mapbayr/reference/get_x.md)
  instead. [\#64](https://github.com/FelicienLL/mapbayr/issues/64)
- The `$model@args$data` is now always `NULL` in the estimation object.
  It was carried out if the data was initially passed with
  [`data_set()`](https://mrgsolve.org/docs/reference/data_set.html) or
  built with
  [`adm_lines()`](https://felicienll.github.io/mapbayr/reference/deprecations.md)/[`obs_lines()`](https://felicienll.github.io/mapbayr/reference/deprecations.md).
  [\#64](https://github.com/FelicienLL/mapbayr/issues/64)
- The time grid used to plot the results is now adapted as function of
  data, and not fixed (refactor of `augment`). Also use `recsort=3` to
  deal with steady-state administrations.
  [\#85](https://github.com/FelicienLL/mapbayr/issues/85)
- Argument passed to
  [`plot()`](https://mrgsolve.org/docs/reference/plot_mrgsims.html) are
  now directly passed to
  [`augment()`](https://felicienll.github.io/mapbayr/reference/augment.md).
- Depends on mrgsolve \>= 1.0.0 to use the newly exported
  [`collapse_omega()`](https://mrgsolve.org/docs/reference/collapse_matrices.html)
  function. (thanks [@kylebaron](https://github.com/kylebaron))

### Experimental features

- Compute and use a normal approximation of conditional distribution.
  The function called in `mapbayest(hessian = )` is used to compute the
  hessian with
  [`stats::optimHess()`](https://rdrr.io/r/stats/optim.html) by default.
  The variance-covariance matrix is returned in a `covariance` slot in
  the estimation object, and can be accessed with
  [`get_cov()`](https://felicienll.github.io/mapbayr/reference/get_x.md).
- Simulate with uncertainty. `use_posterior(update_omega = TRUE)` update
  the OMEGA matrix with the covariance matrix, in order to simulate with
  uncertainty and derive confidence intervals.
- Plot confidence interval. `plot(ci = TRUE)` displays approximate
  confidence intervals on predicted concentrations. Parameter
  uncertainty is approximated with the covariance matrix. Confidence
  interval computation relies on the delta approximation
  (`ci_method = "delta"`), but can also be computed thanks to
  simulations (see
  [`augment()`](https://felicienll.github.io/mapbayr/reference/augment.md)
  documentation).

### New exports

- [`get_cov()`](https://felicienll.github.io/mapbayr/reference/get_x.md):
  function to get the covariance matrix of estimation.
  [\#43](https://github.com/FelicienLL/mapbayr/issues/43)
- [`get_phi()`](https://felicienll.github.io/mapbayr/reference/get_x.md),
  [`read_nmphi()`](https://felicienll.github.io/mapbayr/reference/vs_nonmem.md),
  [`merge_phi()`](https://felicienll.github.io/mapbayr/reference/vs_nonmem.md)
  and
  [`plot_phi()`](https://felicienll.github.io/mapbayr/reference/vs_nonmem.md):
  functions to compare the estimations *vs* NONMEM.
  [\#55](https://github.com/FelicienLL/mapbayr/issues/55)
- `est001`: an example `mapbayests` estimation object.
  [\#94](https://github.com/FelicienLL/mapbayr/issues/94)

### use_posterior()

- add `update_omega`, `update_cov`, and `update_eta` arguments to
  control what to update.
- `.zero_re` default behavior now depends on `update_` arguments values.
- no longer warns if time-varying covariates are used. The first value
  will be used by default.
- now works on multiple individuals: a list of mrgsolve models will be
  returned if multiple individuals found.

### Miscellaneous

- Print a message indicating a difficulty when there is a reset during
  optimization, instead of a warning indicating an error.
  [\#96](https://github.com/FelicienLL/mapbayr/issues/96)
- `mapbayest(verbose = )` now mutes the message that indicates a reset
  during optimization.
  [\#96](https://github.com/FelicienLL/mapbayr/issues/96)
- Remove the attributes of `opt.value` inherited from `optimx`.
  [\#95](https://github.com/FelicienLL/mapbayr/issues/95)
- Detect non-numeric column(s). Stop and inform the user if any.
  [\#86](https://github.com/FelicienLL/mapbayr/issues/86)
  [\#88](https://github.com/FelicienLL/mapbayr/issues/88) (thanks
  [@jkamp91](https://github.com/jkamp91))
- [`get_data()`](https://felicienll.github.io/mapbayr/reference/get_x.md)
  can now return a list of individual data sets with `output = "list"`.
  [\#64](https://github.com/FelicienLL/mapbayr/issues/64)
- Check for undesirable zero in OMEGA/SIGMA matrices instead of
  crashing. [\#44](https://github.com/FelicienLL/mapbayr/issues/44)
- Remove stats from dependencies.
- Add Kyle Baron as contributor.
- Update README since article publication.
- Update documentation.

## mapbayr 0.5.0

CRAN release: 2021-07-27

### Important

- Add new reset conditions: with new initial values if same absolute
  value for every etas, with larger bounds if estimation at bound.
  Additional refactoring about reset as well. see
  [\#75](https://github.com/FelicienLL/mapbayr/issues/75)
- Add an “information” slot to the output, with time records and package
  version [\#69](https://github.com/FelicienLL/mapbayr/issues/69)
- Remove dependency to the `@annotated` tag in model code, especially
  for `$PARAM` and `$CMT` blocks.
  [\#73](https://github.com/FelicienLL/mapbayr/issues/73)
- As a consequence,
  [`adm_lines()`](https://felicienll.github.io/mapbayr/reference/deprecations.md)
  and
  [`obs_lines()`](https://felicienll.github.io/mapbayr/reference/deprecations.md)
  don’t need the \[ADM\] and \[OBS\] tags in model code anymore (yet
  strongly recommended, otherwise it errors cleanly).

### Others

- Update README since first CRAN release
- Check where sigma is equal to zero if error is exponential
  [\#45](https://github.com/FelicienLL/mapbayr/issues/45)
- Use log_transformation() instead of log.transformation()
  [\#24](https://github.com/FelicienLL/mapbayr/issues/24)
- Use unnamed data.frame instead of tibble in get_param()
  [\#77](https://github.com/FelicienLL/mapbayr/issues/77)
- Remove the message when a mapbayests object was passed to plot()
  without augment() before.
  [\#80](https://github.com/FelicienLL/mapbayr/issues/80)
- Don’t stop if no observation in data (no fix, just a test actually)
  [\#23](https://github.com/FelicienLL/mapbayr/issues/23)
- Fix minor `testthat` bugs due to upgrade of R and French translation
  of warnings.
- Fix bug in
  [`plot()`](https://mrgsolve.org/docs/reference/plot_mrgsims.html)
  legend, due to new version of `ggplot2` 3.3.4
  [\#82](https://github.com/FelicienLL/mapbayr/issues/82)

## mapbayr 0.4.1

CRAN release: 2021-04-30

- Fix bugs (dependency, backward compatibility, checks)
- Remove random initial value for method “NEWUOA”. Default to 0.1 for
  each parameter.

## mapbayr 0.4

- More features to hist() function
- Use mapbayest() instead of mbrest()
- Use get_data() instead of see_data()
- Use get_param() to access a posteriori captured parameters
- Use get_eta() to access eta values
- Use use_posterior() to update model with posterior parameters, and
  perform simulations from mapbayests object.

## mapbayr 0.3

### Users :

- Remove arg.ofv from output.
- Add arg.ofv.fix and arg.ofv.id into output. Avoid redundancy and
  decrease the weight of the mbrests object.
- mapbay_tab output improved: return a posteriori captured items and
  covariates (among other)
- Variables passed in dataset cannot be defined in model, except if
  defined with [@covariates](https://github.com/covariates).

### Internal

- Fix and id-varying arguments for ofv processing are dealed separately.
- Data helpers are now ‘mrgmod’ methods
- Maximum reset = 50
- Maximum iteration defaults to 9999
- New ini reset with samples in mvgauss, still respecting l-bfgs-b
  bounds (testthatted).
- Fix bugs [\#41](https://github.com/FelicienLL/mapbayr/issues/41)
  [\#42](https://github.com/FelicienLL/mapbayr/issues/42)
  [\#37](https://github.com/FelicienLL/mapbayr/issues/37)

## mapbayr 0.2.2

- mbraugment(), mbrplot() and mbrhist() are deleted, and replaced by
  augment(), plot() and hist() S3 methods.
- Re-organize internal .R files.
- Rename post process functions.
- Re-write documentation and arguments of mbrest() and its internal
  process. [\#32](https://github.com/FelicienLL/mapbayr/issues/32)

## mapbayr 0.2.1

- Features:
  - Refactor adm_lines() and obs_lines() function. adm_lines() is now
    based on mrgsolve::ev, and can accept “ss” specification. Covered by
    tests.
  - Check for mandatory columns in data set. MDV automatically supplied.
    [\#31](https://github.com/FelicienLL/mapbayr/issues/31)
  - Check the model to see if it fills mapbayr specification. Covered
    with tests
  - \[OBS\] is not mandatory in \$CMT if there is only one compartment
    with observations in the dataset.
- Fix bugs:
  - Throw an error if no dataset is passed.
    [\#29](https://github.com/FelicienLL/mapbayr/issues/29)
  - Refactor MDV == 1 or MDV == 0 behaviour to simulate with every
    lines. [\#30](https://github.com/FelicienLL/mapbayr/issues/30)
  - mbraugment with n compartments \> 1 and n ID \> 1
    [\#33](https://github.com/FelicienLL/mapbayr/issues/33)
- Miscellaneous:
  - Update README
  - Remove some useless functions
  - ofv computation now uses mrgsim_q() faster than basic mrgsim_df
    (theoretically because I did not benchmarked)
  - Added a `NEWS.md` file to track changes to the package.

## mapbayr 0.2.0

- First version in `NEWS.md`
