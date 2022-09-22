test_that("read cmts", {
  mod1 <- mcode("mod1", "
$PARAM DUR = 1
$CMT @annotated
DEPOT : Depot compartment () [ADM]
CENT : Central compartment ()[ADM, OBS]
", compile = FALSE)


  mod2 <- mcode("mod1", "
$PARAM DUR = 1
$CMT @annotated
DEPOT : Depot compartment () [ADM]
CENT : Central compartment ()[OBS]
PERIPH: Central compartment ()
METAB: Central compartment ()[OBS]
", compile = FALSE)

  mod3 <- mcode("mod1", "
$CMT @annotated
CENT : Central compartment ()[ADM, OBS]
", compile = FALSE)

  expect_equal(adm_cmt(mod1), c(1,2))
  expect_equal(adm_cmt(mod2), 1)
  expect_equal(adm_cmt(mod3), 1)

  expect_equal(obs_cmt(mod1), 2)
  expect_equal(obs_cmt(mod2), c(2,4))
  expect_equal(obs_cmt(mod3), 1)
})

test_that("Detection of cmt to fit in data is correct", {

  # === Reminder : how it should work: ===
  # - If [OBS] is provided, it defines fit_cmt.
  ##1) Thus, the length of sigma matrix diagonal must be 2 x number of compartments
  ##2) Thus, obs in data must be defined in model

  # - If [OBS] is not provided, mapbayr will search for one observation compartment on MDV == 0 lines in EACH individual
  ##3) If more than one compartment with observation in the dataset (in one individual) : stop, user should use [OBS] if multiple obs compartments are used
  ##4) Compartment must exist in the model
  ##5) Sigma must be equal to 2

  basecode <- "
$PARAM @annotated
TVCL:  0.5 : Clearance
TVV1: 20.0 : Central volume
V2  : 10.0 : Peripheral volume of distribution
Q   :  3.0 : Intercompartmental clearance
ETA1: 0 : Clearance (L/h)
ETA2: 0 : Central volume (L)
$OMEGA 0.3 0.2
$TABLE
double DV = (CENT/V1) *(1 + EPS(1)) + EPS(2);
$MAIN
double CL = TVCL * exp(ETA1 + ETA(1)) ;
double V1 = TVV1 * exp(ETA2 + ETA(2)) ;
double K12 = Q / V1  ;
double K21 = Q / V2  ;
double K10 = CL / V1 ;
$ODE
dxdt_CENT   =  K21 * PERIPH - (K10 + K12) * CENT ;
dxdt_PERIPH =  K12 * CENT - K21 * PERIPH ;
$CAPTURE DV
  "

  # Let's go. Here [OBS] is defined in $CMT
  sigma0 <- "$SIGMA
0.06 // proportional
0.1 // additive
  "

  cmt0 <- "$CMT @annotated
CENT  : Central compartment (mg/L)[ADM, OBS]
PERIPH: Peripheral compartment ()
  "

  mod1 <- mcode("mod1", code = paste0(basecode, sigma0, cmt0), compile = FALSE)
  data1 <- mod1 %>%
    adm_lines(amt = 100) %>%
    obs_lines(time = 24, DV = 6) %>%
    obs_lines(time = 36, DV = 3) %>%
    get_data()

  #This works
  expect_error(check_mapbayr_modeldata(mod1, data1), NA)

  #1 Modified code to test point 1 = the length of sigma matrix diagonal must be 2 x number of compartments
  sigma1 <- "$SIGMA
0.06 // proportional
0.1 // additive
0.2
  "
  merge_errors <- function(x){paste(x$descr, collapse = " ")}

  mod1 <- mcode("mod1", code = paste0(basecode, sigma1, cmt0), compile = FALSE)

  # expect_match(merge_errors(check_mapbayr_model(mod1, check_compile = FALSE)),
  #              "\\$SIGMA: Define one pair of sigma values.*")

  #2 Modified data to test point 2 = obs cmt in data must be defined in model
  data2 <- mutate(data1, cmt = c(1, 1, 2))
  expect_error(check_mapbayr_modeldata(x = mod1, data2), ".*One or more compartment with observation .* in data don't match those defined with")

  #Other type of model. Here [OBS] is NOT defined in $CMT
  cmt3 <- "$CMT CENT PERIPH"
  mod3 <- mcode("mod3", code = paste0(basecode, sigma0, cmt3), compile = FALSE)

  check3 <- check_mapbayr_model(mod3, check_compile = FALSE)
   expect_equal(fit_cmt(mod3, data1), 1)
  expect_null(obs_cmt(mod3))

  #This works
  expect_error(check_mapbayr_modeldata(mod3, data1), NA)

  ##3) Only one obs compartment in data
  data3 <- mutate(data1, cmt = c(1,2,1)) #here multiple cmt in one patient (ID 1, cmt 1 2)
  expect_error(check_mapbayr_modeldata(mod3, data3), "More than one 'observation compartment' to detect from data. Consider editing model code")

  #also test among two patients (ID 1 cmt 1, ID2 cmt 2) see issue #48
  data31 <- mutate(data1, cmt = c(1,2,2), ID = 20) %>% bind_rows(data1)
  expect_error(check_mapbayr_modeldata(mod3, data31), "More than one 'observation compartment' to detect from data. Consider editing model code")

  # ##4) Compartment must exist in the model
  # data4 <- mutate(data1, cmt = c(1,99,99))
  # expect_error(check_mapbayr_modeldata(mod3, data4), "Compartment number with observation in dataset does not exist in model.")
  # test off: picked up in test-check_mapbayr_modeldata.R

  ##5) Sigma must be equal to 2

  # mod5 <- mcode("mod5", code = paste0(basecode, sigma1, cmt3), compile = FALSE)
  # check5 <- check_mapbayr_model(mod5, check_compile = FALSE)
  # expect_match(merge_errors(check5), ".*Define only one pair of sigma values .* if you do not use .*One observation compartment will be defined from MDV=0 lines in individual data")
})

test_that("eta_descr works", {
  mod87 <- mcode("mod87", "$PARAM ETA1 = 0, ETA2 = 0
$PARAM @annotated @covariate
BW : 50 : Body weight (kg)", compile = FALSE)

expect_equal(eta_descr(mod87), c("ETA1", "ETA2"))

mod87bis <- mcode("mod87bis",
                  "$PARAM @annotated
              ETA1 : 0 : Clearance
              ETA2 : 0 :
$PARAM @annotated @covariate
BW : 50 : Body weight (kg)", compile = FALSE)

expect_equal(eta_descr(mod87bis), c("Clearance", "ETA2"))

mod87ter <- mcode("mod87bis",
                  "$PARAM ETA1 = 0, ETA2 = 0", compile = FALSE)

expect_equal(eta_descr(mod87ter), c("ETA1", "ETA2"))

})
