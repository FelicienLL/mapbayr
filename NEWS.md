# mapbayr development
- More features to hist() function
- Use mapbayest() instead of mbrest()

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
