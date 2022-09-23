test_that("nice error message if we do not pass a mrgsolve model to mapbayest", {
  expect_error(mapbayest("bla"), "the first argument must be a model object")
})

test_that("missing shared object is checked", {
  model_first <- exmodel(cache = FALSE)
  model_second <- exmodel(cache = FALSE)

  expect_error(check_mapbayr_model(model_first, check_compile = TRUE), ".*\\[loadso\\] the model dll file doesn\\'t exist")
  expect_error(check_mapbayr_model(model_first, check_compile = FALSE), NA)

})

mcode2 <- function(code, ...){
  mcode(model = paste(LETTERS[sample.int(26)], collapse = ""),
        code = code,
        compile = FALSE,
        quiet = TRUE,
        cache = FALSE,
        ... = ...)
}

test_that("$PARAM is well-specified", {
  expect_error(
    check_mapbayr_model(mcode2("$PARAM CL = 1"), check_compile = FALSE),
    "\\$PARAM. Cannot find parameters named \\\"ETA1\\\", \\\"ETA2\\\", etc... \nDid you forget to add these parameters in \\$PARAM?"
  )

  expect_error(
    check_mapbayr_model(mcode2("$PARAM ETA1 = 0, ETA3 = 0"), check_compile = FALSE),
    "\\$PARAM. 2 ETA parameter\\(s\\) found, but not named ETA1, ETA2."
  )

  expect_error(
    check_mapbayr_model(mcode2("$PARAM ETA1 = 0, ETA2 = 0.1"), check_compile = FALSE),
    "\\$PARAM. The value of one or multiple ETA parameter\\(s\\) is not 0."
  )
})

test_that("$OMEGA is well-specified", {
  expect_error(
    check_mapbayr_model(mcode2("$PARAM ETA1 = 0, ETA2 = 0
                                $OMEGA 0.1 0.1 0.1"), check_compile = FALSE),
    "\\$OMEGA. The OMEGA matrix diagonal has length 3, but 2 ETA parameters are defined in \\$PARAM"
  )

  expect_error(
    check_mapbayr_model(mcode2("$PARAM ETA1 = 0, ETA2 = 0
                                $OMEGA 0.1 0"), check_compile = FALSE),
    "\\$OMEGA. The value of one or multiple OMEGA value is equal to 0. Cannot accept value in OMEGA equal to zero."
  )
})

test_that("$SIGMA is well-specified", {
  expect_error(
    check_mapbayr_model(mcode2("$PARAM ETA1 = 0, ETA2 = 0
                                $OMEGA 0.1 0.1
                                $SIGMA 0 0"), check_compile = FALSE),
    "\\$SIGMA. All the values in \\$SIGMA are equal to zero, which is not allowed."
  )

  expect_error(
    check_mapbayr_model(mcode2("$PARAM ETA1 = 0, ETA2 = 0
                                $OMEGA 0.1 0.1
                                $SIGMA 1"), check_compile = FALSE),
    "\\$SIGMA. The SIGMA matrix diagonal has length 1. A pair number is expected."
  )

  expect_error(
    check_mapbayr_model(mcode2("$PARAM ETA1 = 0, ETA2 = 0
                                $OMEGA 0.1 0.1
                                $SIGMA 1 2 3 4"), check_compile = FALSE),
    "\\$SIGMA. More than 2 values defined in \\$SIGMA, while \\[OBS\\] was not defined in \\$CMT."
  )

  expect_error(
    check_mapbayr_model(mcode2("$PARAM ETA1 = 0, ETA2 = 0
                                $CMT @annotated
                                PARENT : [OBS]
                                METAB : [OBS]
                                $OMEGA 0.1 0.1
                                $SIGMA 1 2"), check_compile = FALSE),
    "\\$SIGMA. 2 values defined in \\$SIGMA, but 4 were expected. Define one pair of sigma values \\(prop \\+ add errors\\) per \\[OBS\\] compartment\\(s\\) defined in \\$CMT."
  )

  expect_error(
    check_mapbayr_model(mcode2("$PARAM ETA1 = 0, ETA2 = 0
                                $OMEGA 0.1 0.1
                                $SIGMA 1 0
                                $TABLE
                                double DV = exp(EPS(2))"), check_compile = FALSE),
    "\\$SIGMA. Values in position 2,4... \\(i.e. additive\\) cannot be equal to 0 if residual error is defined as exponential in \\$TABLE"
  )

  expect_error(
    check_mapbayr_model(mcode2("$PARAM ETA1 = 0, ETA2 = 0
                                $OMEGA 0.1 0.1
                                $SIGMA 1 1
                                $TABLE
                                double DV = exp(EPS(2)) ;"), check_compile = FALSE),
    "\\$SIGMA. Values in position 1,3...\\(i.e. proportional\\) must be equal to 0 if residual error is defined as exponential in \\$TABLE"
  )

})

test_that("$CAPTURE is well-specified", {
  expect_error(
    check_mapbayr_model(mcode2("$PARAM ETA1 = 0, ETA2 = 0
                               $OMEGA 0.1 0.1
                               $SIGMA 1 0
                               $TABLE double DV = 0 ;"), check_compile = FALSE),
    "\\$CAPTURE. Cannot find DV in captured items. DV must be captured"
  )

  expect_error(
    check_mapbayr_model(mcode2("$PARAM ETA1 = 0, ETA2 = 0
                               $CMT @annotated
                               PARENT : [OBS]
                               METAB : [OBS]
                               $OMEGA 0.1 0.1
                               $SIGMA 1 2 3 4
                               $TABLE
                               double PAR = PARENT ;
                               double MET = METAB ;
                               $CAPTURE DV"), check_compile = FALSE),
    "\\$CAPTURE. Cannot find PAR and MET in captured items. They must be captured if multiple types of DV are fitted \\(more than one pair of sigma provided in \\$SIGMA\\)"
  )

  expect_error(
    check_mapbayr_model(mcode2("$PARAM ETA1 = 0, ETA2 = 0
                               $CMT @annotated
                               PARENT : [OBS]
                               METAB : [OBS]
                               $OMEGA 0.1 0.1
                               $SIGMA 1 2 3 4
                               $TABLE
                               double PAR = PARENT ;
                               double MET = METAB ;
                               $CAPTURE DV MET"), check_compile = FALSE),
    "\\$CAPTURE. Cannot find PAR and MET in captured items. They must be captured if multiple types of DV are fitted \\(more than one pair of sigma provided in \\$SIGMA\\)"
  )

  expect_error(
    check_mapbayr_model(mcode2("$PARAM ETA1 = 0, ETA2 = 0
                               $CMT @annotated
                               PARENT : [OBS]
                               METAB : [OBS]
                               $OMEGA 0.1 0.1
                               $SIGMA 1 2 3 4
                               $TABLE
                               double PAR = PARENT ;
                               double MET = METAB ;
                               $CAPTURE DV PAR"), check_compile = FALSE),
    "\\$CAPTURE. Cannot find PAR and MET in captured items. They must be captured if multiple types of DV are fitted \\(more than one pair of sigma provided in \\$SIGMA\\)"
  )
})

