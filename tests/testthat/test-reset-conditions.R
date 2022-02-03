test_that("check final vs initial OFV", {
  skip_on_cran()
  skip_on_os("mac")
  code <- '$PROB Reference model with IIV on 7 parameters

$PARAM @annotated
TVCL   :  0.2 : Clearance (L/h)
TVKA   :  1.0 : Absorption rate (h-1)
TVVC   :  5.0 : Central volume of distribution (L)
TVVP   : 10.0 : Peripheral volume of distribution (L)
TVF    :  0.8 : Bioavailability ()
TVQ    :  2.0 : Intercompartimental Clearance (L/h)
TVLAG  :  1.5 : Lagtime (h)

ETA1 : 0 : Random effect on CL
ETA2 : 0 : Random effect on VC
ETA3 : 0 : Random effect on KA
ETA4 : 0 : Random effect on VP

$OMEGA
0.4 // CL
0.4 // VC
0.4 // KA
0.4 // VP

$SIGMA
0.05 // err prop
0   //  err additive


$CMT @annotated
DEPOT   : Depot ()       [ADM]
CENTRAL : Central (mg/L) [OBS]
PERIPH  : Peripheral ()  []

$TABLE
double DV = (CENTRAL / VC) * (1 + EPS(1)) + EPS(2) ;

$MAIN
double CL  = TVCL  * exp(ETA(1) + ETA1 ) ;
double VC  = TVVC  * exp(ETA(2) + ETA2 ) ;
double KA  = TVKA  * exp(ETA(3) + ETA3 ) ;
double VP  = TVVP  * exp(ETA(4) + ETA4 ) ;
double F   = TVF   ;
double Q   = TVQ   ;
double LAG = TVLAG ;

double K20 = CL / VC ;
double K23 = Q  / VC ;
double K32 = Q  / VP ;

F_DEPOT = F ;
ALAG_DEPOT = LAG ;

$ODE
dxdt_DEPOT   = - KA * DEPOT ;
dxdt_CENTRAL = - (K20 + K23) * CENTRAL + K32 * PERIPH + KA * DEPOT ;
dxdt_PERIPH  = K23 * CENTRAL - K32 * PERIPH ;


$CAPTURE @annotated
DV : Concentration central
  '

  model <- mrgsolve::mcode('model',code)


  data739 <- model %>%
    adm_lines(amt = 1000, addl = 20, ii = 24) %>%
    obs_lines(time = 40, DV = 76) %>%
    obs_lines(time = 381, DV = 151) %>%
    obs_lines(time = 528, DV = 94) %>%
    get_data()

  #no problem with newuoa :
  est_newuoa <- mapbayest(model, data739, method = "newuoa", verbose = F)
  expect_equal(unname(round(est_newuoa$final_eta[[1]], 4)), c(0.0477, -0.0314, -0.0007, -0.0738))

  #But there is with L-BFGS-B:
  est_lbfgsb0 <- mapbayest(model, data739, method = "L-BFGS-B", verbose = F, reset = F)
  expect_equal(unname(round(est_lbfgsb0$final_eta[[1]], 4)), c(0, 0, 0, 0))
  #With initial eta = 0, this subject automatically converge to... 0.

  #Need a "reset" of the estimation with other values:
  est_lbfgsb_reset <- mapbayest(model, data739, method = "L-BFGS-B", verbose = F, reset = T)
  expect_gt(est_lbfgsb_reset$opt.value$run, 1)
  expect_equal(unname(round(est_lbfgsb_reset$final_eta[[1]], 4)), c(0.0477, -0.0314, -0.0007, -0.0738))

})

test_that("check absolute eta", {
  skip_on_cran()
  code1 <- "
$PARAM @annotated
TVCL : 4.00 : Clearance (L/h)
TVVC : 70.0 : Central volume of distribution (L)
TVKA : 1.00 : Absorption rate (h-1)
ETA1 : 0 : CL
ETA2 : 0 : VC
ETA3 : 0 : KA

$OMEGA 0.2 0.2 0.2
$SIGMA 0.05 0

$CMT @annotated
DEPOT   : Depot () [ADM]
CENTRAL : Central () [OBS]

$TABLE
double DV = (CENTRAL / VC) * (1 + EPS(1)) ;

$MAIN
double CL  = TVCL  * exp(ETA(1) + ETA1) ;
double VC  = TVVC  * exp(ETA(2) + ETA2) ;
double KA  = TVKA  * exp(ETA(3) + ETA3) ;
double K20 = CL / VC ;

$ODE
dxdt_DEPOT   = - KA * DEPOT ;
dxdt_CENTRAL = - K20 * CENTRAL + KA * DEPOT ;

$CAPTURE DV
"
  mod1 <- mrgsolve::mcode("mod1", code1)

  mod1

  data1 <- rbind(
    data.frame(ID = 1, time = c(0, 72), evid = 1, amt = 60000, cmt = 1, ii = c(0,24), addl = c(0,6), mdv = 1, DV = NA_real_),
    data.frame(ID = 1, time = c(215, 217.3, 220.5, 224.9), evid = 0, amt = 0, cmt = 2, ii = 0, addl = 0, mdv = 0, DV = c(46.6047, 1004.58, 699.307, 383.929))
  )

  est1 <- mapbayest(mod1, data1, verbose = F)
  est2 <- mapbayest(mod1, data1, quantile_bound = 0.00001, verbose = F)
  expect_gt(est2$opt.value$run, 1)
  est3 <- mapbayest(mod1, data1, quantile_bound = 0.00001, reset = F, verbose = F)

  expect_true(length(unique(abs(get_eta(est1)))) != 1)
  expect_true(length(unique(abs(get_eta(est2)))) != 1)
  expect_true(length(unique(abs(get_eta(est3)))) == 1)

  expect_equal(get_eta(est1), get_eta(est2), tolerance = .0001)

})

test_that("check bounds", {
  skip_on_cran()
  code1 <- "
$PARAM @annotated
TVCL : 4.00 : Clearance (L/h)
TVVC : 70.0 : Central volume of distribution (L)
TVKA : 1.00 : Absorption rate (h-1)
ETA1 : 0 : CL
ETA2 : 0 : VC
ETA3 : 0 : KA

$OMEGA 0.2 0.2 0.2
$SIGMA 0.05 0

$CMT @annotated
DEPOT   : Depot () [ADM]
CENTRAL : Central () [OBS]

$TABLE
double DV = (CENTRAL / VC) * (1 + EPS(1)) ;

$MAIN
double CL  = TVCL  * exp(ETA(1) + ETA1) ;
double VC  = TVVC  * exp(ETA(2) + ETA2) ;
double KA  = TVKA  * exp(ETA(3) + ETA3) ;
double K20 = CL / VC ;

$ODE
dxdt_DEPOT   = - KA * DEPOT ;
dxdt_CENTRAL = - K20 * CENTRAL + KA * DEPOT ;

$CAPTURE DV
"
  mod1 <- mrgsolve::mcode("mod1", code1)
  data1 <- mod1 %>%
    adm_lines(amt = 100) %>%
    obs_lines(time = c(1, 2, 6, 8), DV = c(0.87, 1.15, 1.07, 0.96)*10) %>% #Observations ten-fold higher than typical profile
    get_data()
  invisible(capture.output(expect_message(est1 <- mapbayest(mod1, data1), "Reset with new bounds")))
  expect_gt(unname(abs(get_eta(est1, 1))), est1$arg.optim$lower[1])

  est2 <- mapbayest(mod1, data1, verbose = FALSE, reset = FALSE)
  expect_equal(unname(get_eta(est2, 1)), est2$arg.optim$lower[1])
})
