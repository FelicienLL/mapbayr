# mapbayr development version
- Detect non-numeric column(s). Stop and inform the user if any. See #86 #88 (thanks @jkamp91)
- Remove stats from dependencies.
- Add Kyle Baron as ctb
- Refactor `augment()`. Automatically chose the time grid for simulations, and use recsort=3 to deal with steady-state administrations. See #85
- Update README since article publication
- Update documentation

# mapbayr 0.5.0
* Important

- Add new reset conditions: with new initial values if same absolute value for every etas, with larger bounds if estimation at bound. Additional refactoring about reset as well. see #75
- Add an "information" slot to the output, with time records and package version #69
- Remove dependency to the `@annotated` tag in model code, especially for `$PARAM` and `$CMT` blocks. #73
- As a consequence, `adm_lines()` and `obs_lines()` don't need the [ADM] and [OBS] tags in model code anymore (yet strongly recommended, otherwise it errors cleanly).

* Others
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

* Users : 
- Remove arg.ofv from output.
- Add arg.ofv.fix and arg.ofv.id into output. Avoid redundancy and decrease the weight of the mbrests object.
- mapbay_tab output improved: return a posteriori captured items and covariates (among other)
- Variables passed in dataset cannot be defined in model, except if defined with @covariates.

* Internal 
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
