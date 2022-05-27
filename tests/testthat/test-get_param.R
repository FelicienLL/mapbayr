code_getparam <-
"
$PROB Reference model

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

$CAPTURE DV CL TVCL
"

mod <- mrgsolve::mcode("mod_getparam", code_getparam)
dat <- exdata(ID = 1:2)
est1 <- mapbayest(mod, dat[dat$ID==1,])
est12 <- mapbayest(mod, dat)

test_that("get param output", {
  #Vector of numeric
  expect_type(get_param(est1, output = "num"), "double")
  expect_error(get_param(est12, output = "num"), "Multiple ID, cannot coerce list to a vector of numeric.")

  #df
  expect_s3_class(get_param(est1, output = "df"), "data.frame")
  expect_s3_class(get_param(est12, output = "df"), "data.frame")

})


test_that("get_param obeys to keep_ argument", {

  expect_equal(get_param(est1, "CL"), 5.9975519)
  expect_equal(get_param(est1, "CL", keep_ID = TRUE), c(1, 5.9975519))
  expect_equal(get_param(est1, "CL", keep_ID = FALSE), 5.9975519)
  expect_equal(get_param(est1, "CL", keep_names = TRUE), c(CL = 5.9975519))
  expect_equal(get_param(est1, "CL", keep_names = FALSE), 5.9975519)
  expect_equal(get_param(est1, "CL", keep_names = TRUE, keep_ID = TRUE), c(ID = 1, CL = 5.9975519))
  expect_equal(get_param(est1, "CL", keep_names = TRUE, keep_ID = FALSE), c(CL = 5.9975519))
  expect_equal(get_param(est1, "CL", keep_names = FALSE, keep_ID = TRUE), c(1, 5.9975519))
  expect_equal(get_param(est1, "CL", keep_names = FALSE, keep_ID = FALSE), 5.9975519)

  expect_equal(get_param(est1, "CL", "TVCL"), c(CL = 5.9975519, TVCL = 4))
  expect_equal(get_param(est1, "CL", "TVCL", keep_ID = TRUE), c(ID = 1, CL = 5.9975519, TVCL = 4))
  expect_equal(get_param(est1, "CL", "TVCL", keep_names = TRUE), c(CL = 5.9975519, TVCL = 4))
  expect_equal(get_param(est1, "CL", "TVCL", keep_names = FALSE), c(5.9975519, 4))
  expect_equal(get_param(est1, "CL", "TVCL", keep_names = TRUE, keep_ID = TRUE), c(ID = 1, CL = 5.9975519, TVCL = 4))
  expect_equal(get_param(est1, "CL", "TVCL", keep_names = TRUE, keep_ID = FALSE), c(CL = 5.9975519, TVCL = 4))
  expect_equal(get_param(est1, "CL", "TVCL", keep_names = FALSE, keep_ID = TRUE), c(1, 5.9975519, 4))
  expect_equal(get_param(est1, "CL", "TVCL", keep_names = FALSE, keep_ID = FALSE), c(5.9975519, 4))

  expect_equal(get_param(est12, "CL"), data.frame(ID = c(1,2), CL = c(5.9975519, 3.4588236)))
  expect_equal(get_param(est12, "CL", keep_ID = TRUE), data.frame(ID = c(1,2), CL = c(5.9975519, 3.4588236)))
  expect_equal(get_param(est12, "CL", keep_ID = FALSE), data.frame(CL = c(5.9975519, 3.4588236)))
  expect_equal(get_param(est12, "CL", keep_names = TRUE), data.frame(ID = c(1,2), CL = c(5.9975519, 3.4588236)))
  expect_equal(get_param(est12, "CL", keep_names = FALSE), unname(data.frame(c(1,2),c(5.9975519, 3.4588236))))
  expect_equal(get_param(est12, "CL", keep_names = TRUE, keep_ID = TRUE), data.frame(ID = c(1,2), CL = c(5.9975519, 3.4588236)))
  expect_equal(get_param(est12, "CL", keep_names = TRUE, keep_ID = FALSE), data.frame(CL = c(5.9975519, 3.4588236)))
  expect_equal(get_param(est12, "CL", keep_names = FALSE, keep_ID = TRUE), unname(data.frame(c(1,2),c(5.9975519, 3.4588236))))
  expect_equal(get_param(est12, "CL", keep_names = FALSE, keep_ID = FALSE), unname(data.frame(c(5.9975519, 3.4588236))))

})

