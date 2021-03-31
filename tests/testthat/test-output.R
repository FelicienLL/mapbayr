test_that("output tab is correct", {

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
$CAPTURE DV CL
"

  mod1 <- mcode("mod1", code1)

  data1 <- data.frame(ID = 1, time = c(0,6,15,24), evid = c(1, rep(0,3)), cmt = 1, amt = c(100, rep(0,3)),
                      rate = c(20, rep(0,3)), DV = c(NA, 3.9, 1.1, 2), BW = 40, DUMMY = 99)

  est1 <- mbrest(mod1, data = data1, output = "df", verbose = F)

  expect_names_1 <- c("ID", "time", "evid", "cmt", "amt", "rate", "DV",
                      "mdv",
                      "BW", "DUMMY",
                      "SEX",
                      "CL", "IPRED", "PRED",
                      "ETA1", "ETA2")

  expect_named(est1, expect_names_1, ignore.order = TRUE)

  expect_equal(as.data.frame(est1)[1,"SEX"], 0)
  expect_equal(as.data.frame(est1)[1,"BW"], 40)
  expect_equal(as.data.frame(est1)[1,"CL"], 0.78314153)


  #Model with metabolite : deal with PAR & MET

  mod2  <- mread("ex_mbr2", mbrlib())

  data2 <- mod2 %>%
    adm_lines(amt = 100, rate = 20) %>%
    obs_lines(time = 6, DV = 5.4, DVmet = 0.8) %>%
    obs_lines(time = 12, DV = 2.6, DVmet = 1.7) %>%
    see_data()

  est2 <- mbrest(mod2, data2, verbose = F, output = "df")

  expect_names_2 <- c("ID", "time", "evid", "mdv", "amt", "rate", "cmt", "DV",
                      "PRED", "IPRED", "PAR", "MET", paste0("ETA", 1:5))

  expect_named(est2, expect_names_2, ignore.order = TRUE)

})


test_that("mbrests object `slots` are correct", {
  mod3 <- mread("ex_mbr3", mbrlib())

  data3 <- bind_rows(
    mod3 %>%
      adm_lines(amt = 100) %>%
      obs_lines(time = 5, DV = 5) %>%
      see_data(),
    mod3 %>%
      adm_lines(amt = 200) %>%
      obs_lines(time = 6, DV = 8) %>%
      see_data() %>%
      mutate(ID = 2)
  )

  est3 <- mbrest(mod3, data3, verbose = F)

  expect_named(est3, c("model", "data", "arg.optim", "arg.ofv.fix", "arg.ofv.id", "opt.value", "final_eta", "mapbay_tab"))
  expect_named(est3$arg.ofv.fix, c("mrgsolve_model", "sigma", "log.transformation", "omega.inv"))
  expect_length(est3$arg.ofv.id, 2)
  expect_named(est3$arg.ofv.id[[1]], c("data", "DVobs", "obs_cmt"))
  expect_named(est3$arg.ofv.id[[2]], c("data", "DVobs", "obs_cmt"))
})
