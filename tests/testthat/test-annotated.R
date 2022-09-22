test_that("annotations are read correctly", {
  code00 <- "
$PROB Demo MAP BAY
$PARAM TVKA = .5, TVCL = 2, TVV = 100
$OMEGA 0.1 0.2 0.15
$SIGMA .05 .1
$CMT DEPOT CENTRAL
$ERROR double DV = (CENTRAL / V) * (1 + EPS(1)) ;
$PK
double KA = TVKA * exp(ETA(1));
double CL = TVCL * exp(ETA(2));
double V  = TVV  * exp(ETA(3));
$CAPTURE DV
$PKMODEL ncmt = 1, depot = TRUE
"
  code0 <- "
$PROB Demo MAP BAY
$PARAM TVKA = .5, TVCL = 2, TVV = 100, ETA1 = 0, ETA2 = 0, ETA3 = 0
$OMEGA 0.1 0.2 0.15
$SIGMA .05 .1
$CMT DEPOT CENTRAL
$ERROR double DV = (CENTRAL / V) * (1 + EPS(1)) ;
$PK
double KA = TVKA * exp(ETA(1) + ETA1);
double CL = TVCL * exp(ETA(2) + ETA2);
double V  = TVV  * exp(ETA(3) + ETA3);
$CAPTURE DV
$PKMODEL ncmt = 1, depot = TRUE
"
  code1 <- "
$PROB Demo MAP BAY
$PARAM @annotated
TVKA : .5 : Typ value KA
TVCL : 2 : Typ value CL
TVV : 100 : Typ value V
ETA1 : 0 : ind value KA
ETA2 : 0 : ind value CL
ETA3 : 0 : ind value V

$OMEGA 0.1 0.2 0.15
$SIGMA .05 .1
$CMT @annotated
DEPOT : Depot [ADM]
CENTRAL : Central [OBS]
$ERROR double DV = (CENTRAL / V) * (1 + EPS(1)) ;
$PK
double KA = TVKA * exp(ETA(1) + ETA1);
double CL = TVCL * exp(ETA(2) + ETA2);
double V  = TVV  * exp(ETA(3) + ETA3);
$CAPTURE DV
$PKMODEL ncmt = 1, depot = TRUE
"
  code2 <- "
$PROB Demo MAP BAY
$PARAM @annotated
TVKA : .5 : Typ value KA
TVCL : 2 : Typ value CL
TVV : 100 : Typ value V
ETA1 : 0 : ind value KA
ETA2 : 0 : ind value CL
ETA3 : 0 : ind value V

$OMEGA 0.1 0.2 0.15
$SIGMA .05 .1
$CMT @annotated
DEPOT : Depot []
CENTRAL : Central []
$INIT @annotated
PD: 123 : pharmacodynamics [OBS]
$ERROR double DV = (CENTRAL / V) * (1 + EPS(1)) ;
$PK
double KA = TVKA * exp(ETA(1) + ETA1);
double CL = TVCL * exp(ETA(2) + ETA2);
double V  = TVV  * exp(ETA(3) + ETA3);
$CAPTURE DV
$ODE
dxdt_DEPOT = - KA * DEPOT ;
dxdt_CENTRAL = KA * DEPOT - (CL/V) * CENTRAL ;
dxdt_PD = PD ;
"
  code3 <- "
$PROB Demo MAP BAY
$PARAM @annotated
TVKA : .5 : Typ value KA
TVCL : 2 : Typ value CL
TVV : 100 : Typ value V
ETA1 : 0 : ind value KA
ETA3 : 0 : ind value V
ETA2 : 0 : ind value CL

$OMEGA 0.1 0.2 0.15
$SIGMA .05 .1
$CMT @annotated
DEPOT : Depot [ADM]
CENTRAL : Central [OBS]
$ERROR double DV = (CENTRAL / V) * (1 + EPS(1)) ;
$PK
double KA = TVKA * exp(ETA(1) + ETA1);
double CL = TVCL * exp(ETA(2) + ETA2);
double V  = TVV  * exp(ETA(3) + ETA3);
$CAPTURE DV
$PKMODEL ncmt = 1, depot = TRUE
"

  data0 <- data.frame(ID = 1,
                      time = c(0, 150),
                      amt = c(100, 0),
                      addl = c(24, 0),
                      ii = c(7, 0),
                      cmt = c(1, 2),
                      DV = c(NA_real_, 1.6),
                      evid = c(1,0),
                      mdv = c(1,0))

  mod00 <- mcode("mod00", code00, compile = FALSE)
  mod0  <- mcode("mod0", code0, compile = FALSE)
  mod1  <- mcode("mod1", code1, compile = FALSE)
  mod2  <- mcode("mod2", code2, compile = FALSE)
  mod3  <- mcode("mod3", code3, compile = FALSE)

  #Model 00 : no ETA defined in $PARAM
  #Model 0 : no annotation
  #Model 1 : annotation with ADM and OBS
  #Model 2 : annotation, but no ADM, just one OBS defined
  #Model 3 : Order switched between ETAs


  # names eta
  expect_null(eta_names(mod00))
  expect_equal(eta_names(mod0), c("ETA1", "ETA2", "ETA3"))
  expect_equal(eta_names(mod1), c("ETA1", "ETA2", "ETA3"))
  expect_equal(eta_names(mod2), c("ETA1", "ETA2", "ETA3"))
  expect_equal(eta_names(mod3), c("ETA1", "ETA3", "ETA2"))

  # description eta
  expect_null(eta_descr(mod00))
  expect_equal(eta_descr(mod0), c("ETA1", "ETA2", "ETA3"))
  expect_equal(eta_descr(mod1), c("ind value KA", "ind value CL", "ind value V"))
  expect_equal(eta_descr(mod2), c("ind value KA", "ind value CL", "ind value V"))
  expect_equal(eta_descr(mod3), c("ind value KA", "ind value V", "ind value CL"))


  #adm_lines
  expect_error(adm_lines(mod00, amt = 100),"Define administration compartment .*")
  expect_error(adm_lines(mod00, amt = 100, cmt = 1), NA)
  expect_error(adm_lines(mod0, amt = 100), "Define administration compartment .*")
  expect_error(adm_lines(mod0, amt = 100, cmt = 1), NA)
  expect_error(adm_lines(mod1, amt = 100), NA)
  expect_error(adm_lines(mod2, amt = 100), "Define administration compartment .*")
  expect_error(adm_lines(mod2, amt = 100, cmt = 1), NA)

  expect_error(adm_lines(mod0, amt = 100, cmt = 1) %>% obs_lines(time = 24, DV = 1.0), "Define observation compartment .*")
  expect_error(adm_lines(mod0, amt = 100, cmt = 1) %>% obs_lines(time = 24, DV = 1.0, cmt = 2), NA)
})

