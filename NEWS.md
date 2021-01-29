# mapbayr (Development version)
* Features: 
  - Refactor adm_lines() and obs_lines() function. adm_lines() is now based on mrgsolve::ev, and can accept "ss" specification. Covered by tests.
  - Check for mandatory columns in data set. MDV automatically supplied.  #31
  - Check the model to see if it fills mapbayr specification
  
* Fix bugs: 
  - Throw an error if no dataset is passed. #29
  - Refactor MDV == 1 or MDV == 0 behaviour to simulate with every lines. #30
  - mbraugment with n compartments > 1 and n ID > 1 #33

* Miscellaneous: 
  - ofv computation now uses mrgsim_q() faster than basic mrgsim_df (theoretically because I did not benchmarked)
  - Added a `NEWS.md` file to track changes to the package.

# mapbayr 0.2.0
* First version in `NEWS.md`
