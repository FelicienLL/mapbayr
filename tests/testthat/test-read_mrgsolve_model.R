test_that("read cmts", {
  mod1 <- mread("ex_mbr1", mbrlib())
  mod2 <- mread("ex_mbr2", mbrlib())
  mod3 <- mread("ex_mbr3", mbrlib())
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
  ##6) mapbayests methods must work too


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

  mod1 <- mcode("mod1", code = paste0(basecode, sigma0, cmt0))
  data1 <- mod1 %>%
    adm_lines(amt = 100) %>%
    obs_lines(time = 24, DV = 6) %>%
    obs_lines(time = 36, DV = 3) %>%
    get_data()

  #This works
  expect_s3_class(mapbayest(mod1, data1, verbose = F), "mapbayests")

  #1 Modified code to test point 1 = the length of sigma matrix diagonal must be 2 x number of compartments
  sigma1 <- "$SIGMA
0.06 // proportional
0.1 // additive
0.2
  "
  mcode("mod1", code = paste0(basecode, sigma1, cmt0)) %>%
    mapbayest(data = data1) %>%
    expect_error(".*SIGMA: Define one pair of sigma values.*")

  #2 Modified data to test point 2 = obs in data must be defined in model
  data2 <- mutate(data1, cmt = c(1, 1, 99))
  expect_error(mapbayest(x = mod1, data2), "ID =1; CMT =99\n One or more compartment with observation .* in data don't match those defined with")

  #Other type of model. Here [OBS] is NOT defined in $CMT
  cmt3 <- "$CMT CENT PERIPH"
  mod3 <- mcode("mod3", code = paste0(basecode, sigma0, cmt3))

  check3 <- check_mapbayr_model(mod3)
  expect_true("$CMT: No [ADM] compartment(s) defined (optionnal)." %in% check3$descr)
  expect_true("$CMT: No [OBS] compartment(s) defined (optionnal)." %in% check3$descr)
  expect_equal(fit_cmt(mod3, data1), 1)
  expect_null(obs_cmt(mod3))

  #This works
  expect_s3_class(est3 <- mapbayest(mod3, data1, verbose = F), "mapbayests")

  ##3) Only one obs compartment in data
  data3 <- mutate(data1, cmt = c(1,2,1)) #here multiple cmt in one patient (ID 1, cmt 1 2)
  expect_error(mapbayest(mod3, data3, verbose = F), "More than one 'observation compartment' to detect from data. Consider editing model code")

  #also test among two patients (ID 1 cmt 1, ID2 cmt 2) see issue #48
  data31 <- mutate(data1, cmt = c(1,2,2), ID = 20) %>% bind_rows(data1)
  expect_error(mapbayest(mod3, data31, verbose = F), "More than one 'observation compartment' to detect from data. Consider editing model code")

  ##4) Compartment must exist in the model
  data4 <- mutate(data1, cmt = c(1,99,99))
  mapbayest(mod3, data4) %>% expect_error("Compartment number with observation in dataset does not exist in model.")

  ##5) Sigma must be equal to 2

  mcode("mod5", code = paste0(basecode, sigma1, cmt3)) %>%
    mapbayest(data = data1) %>%
    expect_error(".*Define only one pair of sigma values .* if you do not use .*One observation compartment will be defined from MDV=0 lines in individual data")

  ##6) Method
  expect_output(print(est3))
  expect_error(as.data.frame(est3), NA)
  expect_error(augment(est3), NA)
  expect_error(plot(est3), NA)
  expect_error(hist(est3), NA)

})


test_that("wrong code is correctly checked", {
  code1 <- "
$PARAM @annotated
TVCL:  0.5 : Clearance
TVV1: 20.0 : Central volume
V2  : 10.0 : Peripheral volume of distribution
Q   :  3.0 : Intercompartmental clearance

ETA1: 0 : Clearance (L/h)
ETA_2: 0 : Central volume (L) //                     error here
ETA3: .1://                                           error here

$OMEGA 0.3 0.2 0.1
$SIGMA
0.06 // proportional
0.1 // additive
0.1 //                                             error here

$CMT CENT PERIPH

$TABLE
double DV = (CENT/V2) *(1 + EPS(1)) + EPS(2);

$MAIN
double CL = TVCL * exp(ETA1 + ETA(1)) ;
double V1 = TVV1 * exp(ETA(2)) ; //                       error
double K12 = (Q / V1) * exp(ETA3 + ETA(3)) ;
double K21 = Q / V2  ;
double K10 = CL / V1 ;

$ODE
dxdt_CENT   =  K21 * PERIPH - (K10 + K12) * CENT ;
dxdt_PERIPH =  K12 * CENT - K21 * PERIPH ;

$CAPTURE DV
"

  check1 <- check_mapbayr_model(mcode("test1", code = code1))

  expect_true("$PARAM: 2 ETA found, but not sequentially named ETA1." %in% check1$descr)
  expect_true("$PARAM: Initial value is not 0 for all ETA." %in% check1$descr)
  expect_true("$PARAM: Description missing for at least one ETA (optionnal)." %in% check1$descr)
  expect_true("$OMEGA: Length of omega matrix diagonal not equal to the number of ETA defined in $PARAM." %in% check1$descr)
  expect_true("$CMT: No [ADM] compartment(s) defined (optionnal)." %in% check1$descr)
  expect_true("$CMT: No [OBS] compartment(s) defined (optionnal)." %in% check1$descr)
  expect_true("$SIGMA: A pair number of sigma values is expected (3 values found)." %in% check1$descr)

})

test_that("model+data are checked", {
  code1 <- "
$PARAM @annotated
TVCL:  0.5 : Clearance
TVV1: 20.0 : Central volume
TVV2: 10.0 : Peripheral volume of distribution
Q   :  3.0 : Intercompartmental clearance

ETA1: 0 : Clearance (L/h)
ETA2: 0 : Central volume (L)
ETA3: 0 : Peripheral volume (L)

$PARAM @annotated @covariates
BW : 70: Body weight (kg)

$OMEGA 0.3 0.2 0.1
$SIGMA 0.06 0.1 0.05 0
$CMT @annotated
CENT: Central volume [ADM, OBS]
PERIPH: Periph volume [OBS]

$TABLE
double PAR = (CENT/V1) *(1 + EPS(1)) + EPS(2) ;
double MET = (PERIPH/V2) * (1+ EPS(3)) + EPS(4) ;
double DV = PAR ;
if(self.cmt == 2) DV = MET ;

$MAIN
double CL = TVCL * exp(ETA1 + ETA(1)) ;
double V1 = TVV1 * exp(ETA2 + ETA(2)) * BW/70 ;
double V2 = TVV1 * exp(ETA3 + ETA(3)) ;

$PKMODEL ncmt = 2, depot = FALSE
$CAPTURE DV PAR MET CL
"
  mod1 <- mcode("mod1", code1)

  data1 <- mod1 %>%
    adm_lines(amt = 100) %>%
    obs_lines(time = 5, DV = 2.2, DVmet = 1.1) %>%
    get_data()

  expect_error(mapbayest(mod1, data1, verbose = FALSE), NA)
  expect_error(mapbayest(mod1, mutate(data1, BW = 10), verbose = FALSE), NA)
  expect_error(mapbayest(mod1, mutate(data1, ETA1 = 1), verbose = FALSE), "These variables cannot be set in both model and data: ETA1")
  expect_error(mapbayest(mod1, mutate(data1, TVCL = 10), verbose = FALSE), "These variables cannot be set in both model and data: TVCL")
  expect_error(mapbayest(mod1, mutate(data1, CL = 10), verbose = FALSE), "These variables cannot be set in both model and data: CL")
  expect_error(mapbayest(mod1, mutate(data1, V1 = 99), verbose = FALSE), "These variables cannot be set in both model and data: V1")

})



test_that("log apply on additive error",{
  code1 <- "$PARAM TVCL = 1, V = 30, ETA1 = 0
$OMEGA 0.3
$SIGMA 0 0.01
$CMT CENT
$MAIN double CL = TVCL * exp(ETA1 + ETA(1)) ;
$TABLE capture DV = CENT/V * exp(EPS(2)) ;
$PKMODEL ncmt = 1, depot = FALSE
"
  code2 <- "$PARAM TVCL = 1, V = 30, ETA1 = 0
$OMEGA 0.3
$SIGMA 0.01 0
$CMT CENT
$MAIN double CL = TVCL * exp(ETA1 + ETA(1)) ;
$TABLE capture DV = CENT/V * exp(EPS(2)) ;
$PKMODEL ncmt = 1, depot = FALSE
"
  code3 <- "$PARAM TVCL = 1, V = 30, ETA1 = 0
$OMEGA 0.3
$SIGMA 0.01 0.01
$CMT CENT
$MAIN double CL = TVCL * exp(ETA1 + ETA(1)) ;
$TABLE capture DV = CENT/V * exp(EPS(2)) ;
$PKMODEL ncmt = 1, depot = FALSE
"
  code4 <- "$PARAM TVCL = 1, V = 30, ETA1 = 0
$OMEGA 0.3
$SIGMA 0 0
$CMT CENT
$MAIN double CL = TVCL * exp(ETA1 + ETA(1)) ;
$TABLE capture DV = CENT/V * exp(EPS(2)) ;
$PKMODEL ncmt = 1, depot = FALSE
"
  mod1 <- mcode("mod1", code1, compile = FALSE)
  mod2 <- mcode("mod2", code2, compile = FALSE)
  mod3 <- mcode("mod3", code3, compile = FALSE)
  mod4 <- mcode("mod4", code4, compile = FALSE)

  descr1 <- check_mapbayr_model(mod1)$descr
  descr2 <- check_mapbayr_model(mod2)$descr
  descr3 <- check_mapbayr_model(mod3)$descr
  descr4 <- check_mapbayr_model(mod4)$descr

  expect_true(!"$SIGMA: Exponential error found. Sigma values in position 2,4... cannot be equal to 0." %in% descr1)
  expect_true(!"$SIGMA: Exponential error found. Sigma values in position 1,3... must be equal to 0." %in% descr1)

  expect_true("$SIGMA: Exponential error found. Sigma values in position 2,4... cannot be equal to 0." %in% descr2)
  expect_true("$SIGMA: Exponential error found. Sigma values in position 1,3... must be equal to 0." %in% descr2)

  expect_true("$SIGMA: Exponential error found. Sigma values in position 1,3... must be equal to 0." %in% descr3)
  expect_true(!"$SIGMA: Exponential error found. Sigma values in position 2,4... cannot be equal to 0." %in% descr3)

  expect_true(!"$SIGMA: Exponential error found. Sigma values in position 1,3... must be equal to 0." %in% descr4)
  expect_true("$SIGMA: Exponential error found. Sigma values in position 2,4... cannot be equal to 0." %in% descr4)

})
