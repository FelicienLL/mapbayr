
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
  expect_warning(est2 <- mapbayest(mod1, data1, quantile_bound = 0.00001, verbose = F))
  est3 <- mapbayest(mod1, data1, quantile_bound = 0.00001, reset = F, verbose = F)

  expect_true(length(unique(abs(get_eta(est1)))) != 1)
  expect_true(length(unique(abs(get_eta(est2)))) != 1)
  expect_true(length(unique(abs(get_eta(est3)))) == 1)

  expect_equal(get_eta(est1), get_eta(est2), tolerance = .0001)

})
