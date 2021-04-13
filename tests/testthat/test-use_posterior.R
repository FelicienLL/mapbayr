code1 <- "
$PARAM @annotated
TVCL:  0.9 : Clearance
TVV1: 10.0 : Central volume
V2  : 10.0 : Peripheral volume of distribution
Q   :  1.0 : Intercompartmental clearance

ETA1: 0 : Clearance (L/h)
ETA2: 0 : Central volume (L)

$PARAM @annotated @covariates
BW : 70 : Body weight (kg)

$OMEGA 0.3 0.3
$SIGMA
0.05 // proportional
0.1 // additive

$CMT @annotated
CENT  : Central compartment (mg/L)[ADM, OBS]
PERIPH: Peripheral compartment ()

$TABLE
double DV = (CENT/V1) *(1 + EPS(1)) + EPS(2);

$MAIN
double CL = TVCL * exp(ETA1 + ETA(1)) * pow(BW / 70, 1.2) ;
double V1 = TVV1 * exp(ETA2 + ETA(2)) ;
double K12 = Q / V1  ;
double K21 = Q / V2  ;
double K10 = CL / V1 ;

$ODE
dxdt_CENT   =  K21 * PERIPH - (K10 + K12) * CENT ;
dxdt_PERIPH =  K12 * CENT - K21 * PERIPH ;

$CAPTURE DV
"
my_model1 <- mcode("Example_model", code1)

my_data1 <- data.frame(ID = 1, time = c(0,6,15,24), evid = c(1, rep(0,3)), cmt = 1, amt = c(100, rep(0,3)),
                       rate = c(20, rep(0,3)), DV = c(NA, 3.9, 1.1, 2), mdv = c(1,0,0,1), BW = 53)

test_that("use_posterior works with cov", {

  my_est1 <- mapbayest(my_model1, my_data1, verbose = F)

  par1 <- my_est1 %>%
    use_posterior() %>%
    param() %>%
    as.list()

  expect_true(par1$ETA1 != 0)
  expect_true(par1$ETA2 != 0)
  expect_equal(par1$BW, 53)

})

code2 <- "
$PARAM @annotated
TVCL:  0.9 : Clearance
TVV1: 10.0 : Central volume
V2  : 10.0 : Peripheral volume of distribution
Q   :  1.0 : Intercompartmental clearance

ETA1: 0 : Clearance (L/h)
ETA2: 0 : Central volume (L)

$OMEGA 0.3 0.3
$SIGMA
0.05 // proportional
0.1 // additive

$CMT @annotated
CENT  : Central compartment (mg/L)[ADM, OBS]
PERIPH: Peripheral compartment ()

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
my_model2 <- mcode("Example_model2", code2)


my_data2 <- data.frame(ID = 1, time = c(0,6,15,24), evid = c(1, rep(0,3)), cmt = 1, amt = c(100, rep(0,3)),
                       rate = c(20, rep(0,3)), DV = c(NA, 3.9, 1.1, 2), mdv = c(1,0,0,1))

my_est2 <- mapbayest(my_model2, my_data2, verbose = F)

test_that("use_posterior works without cov", {

  par2 <- my_est2 %>%
    use_posterior() %>%
    param() %>%
    as.list()

  expect_true(par2$ETA1 != 0)
  expect_true(par2$ETA2 != 0)

})

test_that("zero_re in use_posterior", {
  zero_all <- my_est2 %>%
    use_posterior()

  expect_equal(unname(omat(zero_all, make = T)), diag(c(0,0)))
  expect_equal(unname(smat(zero_all, make = T)), diag(c(0,0)))


  zero_omega <- my_est2 %>%
    use_posterior(.zero_re = "omega")

  expect_equal(unname(omat(zero_omega, make = T)), diag(c(0,0)))
  expect_equal(unname(smat(zero_omega, make = T)), diag(c(0.05,0.1)))


  zero_sigma <- my_est2 %>%
    use_posterior(.zero_re = "sigma")

  expect_equal(unname(omat(zero_sigma, make = T)), diag(c(0.3,0.3)))
  expect_equal(unname(smat(zero_sigma, make = T)), diag(c(0,0)))
})


test_that("multi ID in use_posterior", {

  data12 <- bind_rows(my_data2, mutate(my_data2, ID = 2))
  my_est12 <- mapbayest(my_model2, data12, verbose = F)
  expect_error(use_posterior(my_est12), "use_posterior\\(\\) can be used with one only ID")

})

test_that("warn time-varying cov", {
  my_data1bis <- mutate(my_data1, BW = c(40, 60, 80, 90))

  my_est1bis <- mapbayest(my_model1, my_data1bis, verbose = F)

  expect_warning(  use_posterior(my_est1bis), "Time-varying covariates found. First value used for: BW.")


})
