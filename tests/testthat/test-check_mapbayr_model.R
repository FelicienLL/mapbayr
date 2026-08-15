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
    check_mapbayr_model(mcode2("$PARAM ETA1 = 0, ETA2 = 0"), check_compile = FALSE),
    "Defining ETA1, ETA2... in \\$PARAM is no longer necessary and not allowed"
  )

})

test_that("$OMEGA is well-specified", {
  expect_error(
    check_mapbayr_model(mcode2("$PARAM CL = .1"), check_compile = FALSE),
    "\\ No OMEGA matrix found."
  )
})

test_that("$SIGMA is well-specified", {
  expect_error(
    check_mapbayr_model(mcode2("$OMEGA 0.1 0.1
                                $SIGMA 0 0"), check_compile = FALSE),
    "\\$SIGMA. All the values in \\$SIGMA are equal to zero, which is not allowed."
  )

  expect_error(
    check_mapbayr_model(mcode2("$OMEGA 0.1 0.1
                                $SIGMA 1"), check_compile = FALSE),
    "\\$SIGMA. The SIGMA matrix diagonal has length 1. A pair number is expected."
  )

  expect_error(
    check_mapbayr_model(mcode2("$OMEGA 0.1 0.1
                                $SIGMA 1 2 3 4"), check_compile = FALSE),
    "\\$SIGMA. More than 2 values defined in \\$SIGMA, while \\[OBS\\] was not defined in \\$CMT."
  )

  expect_error(
    check_mapbayr_model(mcode2("$CMT @annotated
                                PARENT : [OBS]
                                METAB : [OBS]
                                $OMEGA 0.1 0.1
                                $SIGMA 1 2"), check_compile = FALSE),
    "\\$SIGMA. 2 values defined in \\$SIGMA, but 4 were expected. Define one pair of sigma values \\(prop \\+ add errors\\) per \\[OBS\\] compartment\\(s\\) defined in \\$CMT."
  )

  expect_error(
    check_mapbayr_model(mcode2("$OMEGA 0.1 0.1
                                $SIGMA 1 0
                                $TABLE
                                double DV = exp(EPS(2))"), check_compile = FALSE),
    "\\$SIGMA. Values in position 2,4... \\(i.e. additive\\) cannot be equal to 0 if residual error is defined as exponential in \\$TABLE"
  )

  expect_error(
    check_mapbayr_model(mcode2("$OMEGA 0.1 0.1
                                $SIGMA 1 1
                                $TABLE
                                double DV = exp(EPS(2)) ;"), check_compile = FALSE),
    "\\$SIGMA. Values in position 1,3...\\(i.e. proportional\\) must be equal to 0 if residual error is defined as exponential in \\$TABLE"
  )

})

test_that("$CAPTURE is well-specified", {
  expect_error(
    check_mapbayr_model(mcode2("$OMEGA 0.1 0.1
                               $SIGMA 1 0
                               $CAPTURE PRED"), check_compile = FALSE),
    "\\$CAPTURE. PRED found in \\$CAPTURE. Do not set PRED in \\$CAPTURE."
  )

  expect_error(
    check_mapbayr_model(mcode2("$OMEGA 0.1 0.1
                               $SIGMA 1 0
                               $CAPTURE IPRED"), check_compile = FALSE),
    "\\$CAPTURE. IPRED found in \\$CAPTURE. Do not set IPRED in \\$CAPTURE."
  )

  expect_error(
    check_mapbayr_model(mcode2("$OMEGA 0.1 0.1
                               $SIGMA 1 0
                               $CAPTURE ETA1"), check_compile = FALSE),
    "\\$CAPTURE. ETAn found in \\$CAPTURE. Do not set ETA1\\, ETA2 etc... in \\$CAPTURE."
  )

  expect_error(
    check_mapbayr_model(mcode2("$OMEGA 0.1 0.1
                               $SIGMA 1 0
                               $TABLE double DV = 0 ;"), check_compile = FALSE),
    "\\$CAPTURE. Cannot find DV in captured items. DV must be captured"
  )

  expect_error(
    check_mapbayr_model(mcode2("$CMT @annotated
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
    check_mapbayr_model(mcode2("$CMT @annotated
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
    check_mapbayr_model(mcode2("$CMT @annotated
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

test_that("has_eta_param() works", {
  expect_true(has_eta_param(mcode2("
  $PARAM ETA1 = 0, ETA2 = 0
  $OMEGA 1 2 3")
  ))

  expect_false(has_eta_param(mcode2("
  $PARAM CL = 1, V2 = 30
  $OMEGA 1 2 3")
  ))
})
