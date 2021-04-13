
code1 <- "
$PARAM @annotated
TVCL:  1 : Clearance
TVV1: 10.0 : Central volume
V2  : 10.0 : Peripheral volume of distribution
Q   :  1.0 : Intercompartmental clearance
BW_CL : 1.2: Body weight effect on CL
SEX_CL : 1.2: Sex effect on CL

ETA1: 0 : Clearance (L/h)
ETA2: 0 : Central volume (L)

$PARAM @annotated @covariates
BW : 70 : Body weight (kg)
SEX: 0 : Sex (0 male, 1 female)

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
double CL = TVCL * exp(ETA1 + ETA(1)) * pow(BW / 70, BW_CL) * pow(SEX_CL, SEX) ;
double V1 = TVV1 * exp(ETA2 + ETA(2)) ;
$PKMODEL ncmt = 2, depot = FALSE
$CAPTURE TVCL CL DV
"

mod1 <- mcode("mod1", code1)

data1 <- data.frame(ID = 1, time = c(0,6,15,24), evid = c(1, rep(0,3)), cmt = 1, amt = c(100, rep(0,3)),
                    rate = c(20, rep(0,3)), DV = c(NA, 3.9, 1.1, 2), BW = 40, DUMMY = 99)
data12 <- bind_rows(data1, mutate(data1, ID = 2, BW = 90))
est1 <- mapbayest(mod1, data = data1, verbose = F)
est12 <- mapbayest(mod1, data = data12, verbose = F)

CL1 <- est12$mapbay_tab$CL[1]
CL2 <- est12$mapbay_tab$CL[5]


test_that("get param output", {
  #Vector of numeric
  expect_type(get_param(est1, output = "num"), "double")
  expect_error(get_param(est12, output = "num"), "Multiple ID, cannot coerce list to a vector of numeric.")

  #df
  expect_s3_class(get_param(est1, output = "df"), "data.frame")
  expect_s3_class(get_param(est12, output = "df"), "data.frame")

})


test_that("get_param obeys to keep_ argument", {

  expect_equal(get_param(est1, "CL"), CL1)
  expect_equal(get_param(est1, "CL", keep_ID = TRUE), c(1, CL1))
  expect_equal(get_param(est1, "CL", keep_ID = FALSE), CL1)
  expect_equal(get_param(est1, "CL", keep_names = TRUE), c(CL = CL1))
  expect_equal(get_param(est1, "CL", keep_names = FALSE), CL1)
  expect_equal(get_param(est1, "CL", keep_names = TRUE, keep_ID = TRUE), c(ID = 1, CL = CL1))
  expect_equal(get_param(est1, "CL", keep_names = TRUE, keep_ID = FALSE), c(CL = CL1))
  expect_equal(get_param(est1, "CL", keep_names = FALSE, keep_ID = TRUE), c(1, CL1))
  expect_equal(get_param(est1, "CL", keep_names = FALSE, keep_ID = FALSE), CL1)

  expect_equal(get_param(est1, "CL", "TVCL"), c(CL = CL1, TVCL = 1))
  expect_equal(get_param(est1, "CL", "TVCL", keep_ID = TRUE), c(ID = 1, CL = CL1, TVCL = 1))
  expect_equal(get_param(est1, "CL", "TVCL", keep_names = TRUE), c(CL = CL1, TVCL = 1))
  expect_equal(get_param(est1, "CL", "TVCL", keep_names = FALSE), c(CL1, 1))
  expect_equal(get_param(est1, "CL", "TVCL", keep_names = TRUE, keep_ID = TRUE), c(ID = 1, CL = CL1, TVCL = 1))
  expect_equal(get_param(est1, "CL", "TVCL", keep_names = TRUE, keep_ID = FALSE), c(CL = CL1, TVCL = 1))
  expect_equal(get_param(est1, "CL", "TVCL", keep_names = FALSE, keep_ID = TRUE), c(1, CL1, 1))
  expect_equal(get_param(est1, "CL", "TVCL", keep_names = FALSE, keep_ID = FALSE), c(CL1, 1))

  expect_equal(get_param(est12, "CL"), tibble(ID = c(1,2), CL = c(CL1, CL2)))
  expect_equal(get_param(est12, "CL", keep_ID = TRUE), tibble(ID = c(1,2), CL = c(CL1, CL2)))
  expect_equal(get_param(est12, "CL", keep_ID = FALSE), tibble(CL = c(CL1, CL2)))
  expect_equal(get_param(est12, "CL", keep_names = TRUE), tibble(ID = c(1,2), CL = c(CL1, CL2)))
  expect_equal(get_param(est12, "CL", keep_names = FALSE), unname(tibble(c(1,2),c(CL1, CL2))))
  expect_equal(get_param(est12, "CL", keep_names = TRUE, keep_ID = TRUE), tibble(ID = c(1,2), CL = c(CL1, CL2)))
  expect_equal(get_param(est12, "CL", keep_names = TRUE, keep_ID = FALSE), tibble(CL = c(CL1, CL2)))
  expect_equal(get_param(est12, "CL", keep_names = FALSE, keep_ID = TRUE), unname(tibble(c(1,2),c(CL1, CL2))))
  expect_equal(get_param(est12, "CL", keep_names = FALSE, keep_ID = FALSE), unname(tibble(c(CL1, CL2))))

})





