test_that("L-BFGS-B changes eta values", {
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
    see_data()

  #no problem with newuoa :
  est_newuoa <- mapbayest(model, data739, method = "newuoa", verbose = F)
  expect_equal(unname(round(est_newuoa$final_eta[[1]], 4)), c(0.0477, -0.0314, -0.0007, -0.0738))

  #But there is with L-BFGS-B:
  est_lbfgsb0 <- mapbayest(model, data739, method = "L-BFGS-B", verbose = F, reset = F)
  expect_equal(unname(round(est_lbfgsb0$final_eta[[1]], 4)), c(0, 0, 0, 0))
  #With initial eta = 0, this subject automatically converge to... 0.

  #Need a "reset" of the estimation with other values:
  expect_warning(est_lbfgsb_reset <- mapbayest(model, data739, method = "L-BFGS-B", verbose = F, reset = T), "Error in optimization. Reset with initial values:")
  expect_equal(unname(round(est_lbfgsb_reset$final_eta[[1]], 4)), c(0.0477, -0.0314, -0.0007, -0.0738))

})
