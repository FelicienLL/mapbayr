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

my_est1 <- mapbayest(my_model1, my_data1, verbose = F)

test_that("use_posterior works with cov", {
  par1 <- my_est1 %>%
    use_posterior() %>%
    param() %>%
    as.list()

  expect_true(par1$ETA1 != 0)
  expect_true(par1$ETA2 != 0)
  expect_equal(par1$BW, 53)

})


test_that("use_posterior obeys to update_x arguments", {

  defaultposterior <- use_posterior(my_est1, .zero_re = "none")

  expect_equal(defaultposterior$ETA1, 0.90870594)
  expect_equal(defaultposterior$BW, 53)
  defaultposterior %>%
    omat(make = TRUE) %>%
    expect_equal(diag(c(0.3,0.3)))

  my_est1 %>%
    use_posterior(update_omega = FALSE, .zero_re = "none") %>%
    omat(make = TRUE) %>%
    expect_equal(diag(c(0.3,0.3)))

  my_est1 %>%
    use_posterior(update_omega = TRUE, .zero_re = "none") %>%
    omat(make = TRUE) %>%
    expect_equal(matrix(c(0.05838635, 0.01262452, 0.01262452, 0.21315263), nrow = 2))

  expect_equal(use_posterior(my_est1, update_cov = FALSE)$BW, 70)
  expect_equal(use_posterior(my_est1, update_cov = TRUE)$BW, 53)

  expect_equal(use_posterior(my_est1, update_eta = FALSE)$ETA1, 0)
  expect_equal(use_posterior(my_est1, update_eta = TRUE)$ETA1, 0.90870594)

})




code2 <- "
$PARAM @annotated
TVCL:  0.9 : Clearance
TVV1: 10.0 : Central volume
V2  : 10.0 : Peripheral volume of distribution
Q   :  1.0 : Intercompartmental clearance

ETA1: 0 : Clearance (L/h)
ETA2: 0 : Central volume (L)

$PARAM @covariates
BW = 70

$OMEGA 0.3
$OMEGA 0.3
$SIGMA
0.05 // proportional
0.1 // additive

$CMT @annotated
CENT  : Central compartment (mg/L)[ADM, OBS]
PERIPH: Peripheral compartment ()

$TABLE
double DV = (CENT/V1) *(1 + EPS(1)) + EPS(2);

$MAIN
double CL = TVCL * exp(ETA1 + ETA(1)) * (BW/70.0) ;
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
                       rate = c(20, rep(0,3)), DV = c(NA, 3.9, 1.1, 2), mdv = c(1,0,0,1), BW = 35)

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

  zero_none <- my_est2 %>%
    use_posterior(.zero_re = "none")

  expect_equal(unname(omat(zero_none, make = T)), diag(c(0.3,0.3)))
  expect_equal(unname(smat(zero_none, make = T)), diag(c(0.05,0.1)))

  zero_covariance <- my_est2 %>%
    use_posterior(update_omega = TRUE)

  expect_equal(unname(omat(zero_covariance, make = T)), get_cov(my_est2))
  expect_equal(unname(smat(zero_covariance, make = T)), diag(c(0,0)))

})

test_that("multi ID", {

  data12 <- bind_rows(my_data2, mutate(my_data2, ID = 2, BW = 150))
  my_est12 <- mapbayest(my_model2, data12, verbose = F)
  post12 <- my_est12 %>% use_posterior(update_omega = TRUE)

  expect_length(post12, 2)

  expect_equal(omat(post12[[1]], make = TRUE), matrix(c(0.069320257, -0.003361061, -0.003361061, 0.258101901), nrow = 2))
  expect_equal(omat(post12[[2]], make = TRUE), matrix(c(0.04727953, 0.02075817, 0.02075817, 0.15242651), nrow = 2))

  expect_equal(post12[[1]]$ETA1, 1.192502)
  expect_equal(post12[[2]]$ETA1, 0.001407662)

  expect_equal(post12[[1]]$BW, 35)
  expect_equal(post12[[2]]$BW, 150)

})

# Do not warn systematically since use_posterior is also internal now...
# test_that("warn time-varying cov", {
#   my_data1bis <- mutate(my_data1, BW = c(40, 60, 80, 90))
#
#   my_est1bis <- mapbayest(my_model1, my_data1bis, verbose = F)
#
#   expect_warning(  use_posterior(my_est1bis), "Time-varying covariates found. First value used for: BW.")
#
#
# })

