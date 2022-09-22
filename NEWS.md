# mapbayr 0.7.3.9006 (Development version)
- New functions `summarise_phi()` and `bar_phi()` to summarise the comparison of estimation of mapbayr and NONMEM (i.e. classify it as Excellent/Acceptable/Discordant), and to graphically represent it as a bar plot. 
- Fix a bug where "vs_nonmem" functions could not work if covariance was missing/failing in mapbayests object.
- Fix a bug where small negative predicted concentrations generated NaN after log-transformation. #140
- Post-processing was rewritten to be more efficient and in now directly executed after optimization by `mapbayest()` as function of the `output =` argument. #134
- `postprocess.optim` and `postprocess.output` are not exported anymore. 
- Data splitting is simpler, #127
- the `reset` argument is now a numeric and drives the maximum allowed reset during optimization.
- optimization is now done by calling `stats::optim` if method is L-BFGS-B and `minqa::newuoa` if method is newuoa. These replace `optimx::optimx`. #136 
- Remove dependency to `optimx` package.
- Fix a bug where objective function value could not be computed if lag time was longer than interdose interval at steady-state because it is a known error of 'mrgsolve'. #142
- Forces progress bar to appear, especially in 'RStudio' job launcher.
- Fix a bug where non-loaded shared object were not signaled explicitely #130
- Fix a bug where missing values in `mdv==0` lines were not checked #131
- Fix a bug where compartment in the data were not defined in the model #132
- `check_mapbayr_model()` now returns an error if a check fails instead of a table that summarised the check.
- `check_mapbayr_model()` now only checks critical points and not suggested features.
- Fix a bug where the model object type was not checked before being used in `mapbayest()` #149
- Better error message if covariates are not properly tagged in the model #92
- Bug fix: now detects error as exponential even if sigma labels are used to define `DV` #150
- Test refactor and more tests for internal *_cmt functions

# mapbayr 0.7.3
- Minor changes in DESCRIPTION file (CRAN requirements)

# mapbayr 0.7.2
- Change contact address in description (CRAN requirements)

# mapbayr 0.7.1
- Fix additional CRAN checks (Mac M1)

# mapbayr 0.7.0

## Breaking changes
- Change the outputs of pre-processing functions. For fixed elements, `qmod`, `omega_inv `and `all_cmt` now replace `mrgsolve_model`, `omega.inv` and `obs_cmt`. For individual-related elements, `idDV` replaces `DVobs`, `data` is removed, `idvaliddata` and `idcmt` are added. This can have an impact for the user since these elements are reported in the standard output. However, it does not change the behaviour of `get_data()`.
- Change argument behaviour: `mapbayest(verbose = TRUE)` now only displays the messages related to optimization reset, and not the progression of ID being optimized which is now controlled by `mapbayest(progress = TRUE)`.
- Stop exporting `derivatives()`, now replaced by `mapbayr:::h()`.
- Stop exporting `mbrlib()` and associated models. See the "Model examples" section below.

## Model examples
The example models system was totally re-thought around a new function: `exmodel()`. It now embeds several models that were used in the validation study, with a small corresponding dataset that can be loaded automatically (the default). They are used in multiple places inside the package, especially in tests and examples. More models could be added in the future.  

- Export `exmodel()`. See the list of available models in the documentation.  
- Export `exdata()`, to load data only.

## Miscellaneous
- New argument: `mapbayest(progress = TRUE)` displays a progress bar with the number of the ID being optimized. #118 #28
- New argument value: `mapbayest(output = "eta")` returns only estimated ETA in order to skip most of post-processing steps. #106
- `eta_descr()` now always returns a non-NA value even if description is missing. #87
- New function: `do_compute_ofv()`, a wrapper around `do.call(compute_ofv, ...)`.
- Add Dependency: `{progress}`.
- Add Suggestion: `{testthat}`, `{minqa}`. #120
- Improve the performance of objective function value calculation. Now use a faster parameter update, pre-validate data and refactor the computation of the H matrix. Thanks @kylebaron for the useful suggestions. #104 #111
- Improve tests. Now work with example models which is overall lighter, faster, more consistent, more unitary. Also, they are run during `R CMD check`.
- Improve documentation: some monographs were merged, and some gained an example section thanks to the new example models system.

## Bug fixes
- `plot_phi()` now plots correct values on the x-axis. #108
- No warning when plotting data with DV being `NA`. #114
- No warning when updating a model without covariates. #115
- No systematic reset if one "ETA" to estimate. #116
- No errors at the end of reset-related messages. #119

# mapbayr 0.6.0
This version of mapbayr introduces several features that aim to express uncertainty around the point estimate. Please note that the results of these functions were not validated *vs* a gold-standard software such as NONMEM. This is why they are referred as "experimental features" in the following subsections. They are exported with the objective to ease their future validation, and to provide a very rough idea of the estimation uncertainty.

## Breaking changes
- Remove `data` slot in estimation object. Use `get_data()` instead. #64
- The `$model@args$data` is now always `NULL` in the estimation object. It was carried out if the data was initially passed with `data_set()` or built with `adm_lines()`/`obs_lines()`. #64
- The time grid used to plot the results is now adapted as function of data, and not fixed (refactor of `augment`). Also use `recsort=3` to deal with steady-state administrations. #85
- Argument passed to `plot()` are now directly passed to `augment()`.
- Depends on mrgsolve >= 1.0.0 to use the newly exported `collapse_omega()` function. (thanks @kylebaron)

## Experimental features
- Compute and use a normal approximation of conditional distribution. The function called in `mapbayest(hessian = )` is used to compute the hessian with `stats::optimHess()` by default. The variance-covariance matrix is returned in a `covariance` slot in the estimation object, and can be accessed with `get_cov()`. 
- Simulate with uncertainty. `use_posterior(update_omega = TRUE)` update the OMEGA matrix with the covariance matrix, in order to simulate with uncertainty and derive confidence intervals.
- Plot confidence interval. `plot(ci = TRUE)` displays approximate confidence intervals on predicted concentrations. Parameter uncertainty is approximated with the covariance matrix. Confidence interval computation relies on the delta approximation (`ci_method = "delta"`), but can also be computed thanks to simulations (see `augment()` documentation).

## New exports
- `get_cov()`: function to get the covariance matrix of estimation. #43
- `get_phi()`, `read_nmphi()`, `merge_phi()` and `plot_phi()`: functions to compare the estimations *vs* NONMEM. #55
- `est001`: an example `mapbayests` estimation object. #94

## use_posterior()
- add `update_omega`, `update_cov`, and `update_eta` arguments to control what to update. 
- `.zero_re` default behavior now depends on `update_` arguments values. 
- no longer warns if time-varying covariates are used. The first value will be used by default.
- now works on multiple individuals: a list of mrgsolve models will be returned if multiple individuals found.

## Miscellaneous
- Print a message indicating a difficulty when there is a reset during optimization, instead of a warning indicating an error. #96
- `mapbayest(verbose = )` now mutes the message that indicates a reset during optimization. #96
- Remove the attributes of `opt.value` inherited from `optimx`. #95
- Detect non-numeric column(s). Stop and inform the user if any. #86 #88 (thanks @jkamp91)
- `get_data()` can now return a list of individual data sets with `output = "list"`. #64
- Check for undesirable zero in OMEGA/SIGMA matrices instead of crashing. #44
- Remove stats from dependencies.
- Add Kyle Baron as contributor.
- Update README since article publication.
- Update documentation.

# mapbayr 0.5.0
## Important

- Add new reset conditions: with new initial values if same absolute value for every etas, with larger bounds if estimation at bound. Additional refactoring about reset as well. see #75
- Add an "information" slot to the output, with time records and package version #69
- Remove dependency to the `@annotated` tag in model code, especially for `$PARAM` and `$CMT` blocks. #73
- As a consequence, `adm_lines()` and `obs_lines()` don't need the [ADM] and [OBS] tags in model code anymore (yet strongly recommended, otherwise it errors cleanly).

## Others
- Update README since first CRAN release
- Check where sigma is equal to zero if error is exponential #45
- Use log_transformation() instead of log.transformation() #24
- Use unnamed data.frame instead of tibble in get_param() #77
- Remove the message when a mapbayests object was passed to plot() without augment() before. #80
- Don't stop if no observation in data (no fix, just a test actually) #23
- Fix minor `testthat` bugs due to upgrade of R and French translation of warnings.
- Fix bug in `plot()` legend, due to new version of `ggplot2` 3.3.4 #82

# mapbayr 0.4.1
- Fix bugs (dependency, backward compatibility, checks)
- Remove random initial value for method "NEWUOA". Default to 0.1 for each parameter.

# mapbayr 0.4

- More features to hist() function
- Use mapbayest() instead of mbrest()
- Use get_data() instead of see_data()
- Use get_param() to access a posteriori captured parameters
- Use get_eta() to access eta values
- Use use_posterior() to update model with posterior parameters, and perform simulations from mapbayests object.

# mapbayr 0.3

## Users : 
- Remove arg.ofv from output.
- Add arg.ofv.fix and arg.ofv.id into output. Avoid redundancy and decrease the weight of the mbrests object.
- mapbay_tab output improved: return a posteriori captured items and covariates (among other)
- Variables passed in dataset cannot be defined in model, except if defined with @covariates.

## Internal 
- Fix and id-varying arguments for ofv processing are dealed separately.
- Data helpers are now 'mrgmod' methods
- Maximum reset = 50
- Maximum iteration defaults to 9999
- New ini reset with samples in mvgauss, still respecting l-bfgs-b bounds (testthatted).
- Fix bugs #41 #42 #37

# mapbayr 0.2.2

- mbraugment(), mbrplot() and mbrhist() are deleted, and replaced by augment(), plot() and hist() S3 methods.
- Re-organize internal .R files.
- Rename post process functions.
- Re-write documentation and arguments of mbrest() and its internal process. #32

# mapbayr 0.2.1
* Features: 
  - Refactor adm_lines() and obs_lines() function. adm_lines() is now based on mrgsolve::ev, and can accept "ss" specification. Covered by tests.
  - Check for mandatory columns in data set. MDV automatically supplied.  #31
  - Check the model to see if it fills mapbayr specification. Covered with tests
  - [OBS] is not mandatory in $CMT if there is only one compartment with observations in the dataset.
  
* Fix bugs: 
  - Throw an error if no dataset is passed. #29
  - Refactor MDV == 1 or MDV == 0 behaviour to simulate with every lines. #30
  - mbraugment with n compartments > 1 and n ID > 1 #33

* Miscellaneous: 
  - Update README
  - Remove some useless functions
  - ofv computation now uses mrgsim_q() faster than basic mrgsim_df (theoretically because I did not benchmarked)
  - Added a `NEWS.md` file to track changes to the package.

# mapbayr 0.2.0
* First version in `NEWS.md`
