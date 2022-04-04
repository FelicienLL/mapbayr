test_that("MDV == 0 are handled properly", {
 code3 <-  "
  $PROB
- drug: Examplinib3
- model_ref: XXX, J Pharmacokinet, 2020

$PARAM @annotated
TVCL   : .7 : Clearance (volume/time)
TVV1   : 20 : Central volume (volume)
TVV2   : 10 : Peripheral volume of distribution (volume)
Q      :  3 : Inter-compartmental clearance (volume/time)

ETA1 : 0 : Clearance (L/h)
ETA2 : 0 : Central volume (L)
ETA3 : 0 : Peripheral volume (L)

$PARAM @annotated @covariates

COV : 100 : Covariate (unit)
$OMEGA 0.3 0.2 0.1
$SIGMA 0.06 0

$CMT @annotated
CENT   : Central compartment (mg/L)[ADM, OBS]
PERIPH : Peripheral compartment ()

$TABLE
double DV = (CENT/V2) *(1 + EPS(1)) ;

$MAIN
double CL = TVCL * exp(ETA1 + ETA(1)) * (COV/100) ;
double V1 = TVV1 * exp(ETA2 + ETA(2)) ;
double V2 = TVV2 * exp(ETA3 + ETA(3)) ;

$PKMODEL ncmt = 2

$CAPTURE @annotated
DV : Plasma concentration (mass/time)
  "


  mod3 <- mcode("ex_mbr3", code3)
  base_data <- mod3 %>%
    adm_lines(amt = 100) %>%
    obs_lines(time = 11, DV = 3) %>%
    obs_lines(time = 12, DV = 3) %>%
    obs_lines(time = 24, DV = 1) %>%
    get_data() #Three obs taken into account (1)

  data_MDV1 <- base_data %>%
    mutate(mdv = c(1,1,0,0))  #Two obs taken into account, one ignored (2)

  data_MDV1_CL <- data_MDV1 %>%
    mutate(COV = c(100, 200, 100, 100)) #Still one ignored , but another info on CL (3) :the line must be read by mrgsim, but not for OFV

  data_noline_CL <- data_MDV1_CL %>%
    filter(time != 11) #The line is just dropped (4)

  est1 <- mapbayest(mod3, data = base_data, verbose = F)
  est2 <- mapbayest(mod3, data = data_MDV1, verbose = F)
  est3 <- mapbayest(mod3, data = data_MDV1_CL, verbose = F)
  est4 <- mapbayest(mod3, data = data_noline_CL, verbose = F)

  #Test number of observation fitted
  expect_equal(length(est1$arg.ofv.id[[1]]$idDV), 3)
  expect_equal(length(est2$arg.ofv.id[[1]]$idDV), 2)
  expect_equal(length(est3$arg.ofv.id[[1]]$idDV), 2)
  expect_equal(length(est4$arg.ofv.id[[1]]$idDV), 2)

  #Test different eta are found
  # 1 != 2
  # 1 != 3
  # 1 != 4
  # 2 != 3
  # 2 == 4
  # 3 != 4

  expect_false(isTRUE(all.equal(est1$final_eta[[1]], est2$final_eta[[1]])))
  expect_false(isTRUE(all.equal(est1$final_eta[[1]], est3$final_eta[[1]])))
  expect_false(isTRUE(all.equal(est1$final_eta[[1]], est4$final_eta[[1]])))
  expect_false(isTRUE(all.equal(est2$final_eta[[1]], est3$final_eta[[1]])))
  expect_true(isTRUE(all.equal(est2$final_eta[[1]], est4$final_eta[[1]])))
  expect_false(isTRUE(all.equal(est3$final_eta[[1]], est4$final_eta[[1]])))
  })
