  code1 <-  "$PROB Reference model

$PARAM @annotated
TVCL   : 4.00 : Clearance (L/h)
TVVC   : 70.0 : Central volume of distribution (L)
TVKA   : 1.00 : Absorption rate (h-1)

ETA1 : 0 : CL
ETA2 : 0 : VC
ETA3 : 0 : KA

$OMEGA
0.2 // CL
0.2 // VC
0.2 // KA

$SIGMA
0.05 // err prop
0   //  err additive


$CMT @annotated
DEPOT   : Depot () [ADM]
CENTRAL : Central () [OBS]

$TABLE
double DV = (CENTRAL / VC) * (1 + EPS(1)) + EPS(2) ;

$MAIN
double CL  = TVCL  * exp(ETA(1) + ETA1 ) ;
double VC  = TVVC  * exp(ETA(2) + ETA2 ) ;
double KA  = TVKA  * exp(ETA(3) + ETA3 ) ;

double K20 = CL / VC ;

$ODE
dxdt_DEPOT   = - KA * DEPOT ;
dxdt_CENTRAL = - K20 * CENTRAL + KA * DEPOT ;

$CAPTURE DV
  "

  model1 <- mcode("model1", code1)
  data1 <- model1 %>%
    adm_lines(amt = 10000) %>%
    obs_lines(time = c(1.5, 4.4, 7.1, 24.6), DV = c(91.2904, 110.826, 79.384,20.6671)) %>%
    get_data()
  data2 <- bind_rows(data1, mutate(data1, ID = 99, DV = 0.8*DV))
  est1 <- mapbayest(model1, data1, verbose = FALSE)
  est2 <- mapbayest(model1, data2, verbose = FALSE)

test_that("covariance matrix is correct", {
  est0 <- mapbayest(model1, data1, hessian = FALSE, verbose = FALSE)
  expect_true(is.na(est0$covariance))

 # est1b <- mapbayest(model1, data1, verbose = FALSE, hessian = "nlmixrHess")

  nmphi <- matrix(c(1.28120118E-002, 5.40868557E-003, 4.69547364E-004,
                    5.40868557E-003, 2.31664035E-002, 2.19133609E-002,
                    4.69547364E-004, 2.19133609E-002, 1.25252672E-001), nrow = 3, ncol = 3)

  expect_equal(est1$covariance[[1]], nmphi, tolerance = 0.01)
 # expect_equal(est1b$covariance[[1]], nmphi, tolerance = 0.01)
})

test_that("get_cov method is correct", {
  expect_equal(get_cov(est1), est1$covariance[[1]])
  expect_length(unique(data2$ID), 2)
  expect_equal(get_cov(est2), list(`1` = est2$covariance[[1]], `99`= est2$covariance[[2]]))
  expect_equal(get_cov(est1, simplify = FALSE), list(`1` = est2$covariance[[1]]))
})

test_that("get_phi works", {
  expect_named(phi1 <- get_phi(est1), c("SUBJECT_NO", "ID", "ETA1", "ETA2", "ETA3", "ETC1_1", "ETC2_1", "ETC2_2", "ETC3_1", "ETC3_2", "ETC3_3", "OBJ"))
  phi2 <- get_phi(est2)
  expect_equal(nrow(phi2), 2)
  expect_equal(phi1, phi2[1,])
})
