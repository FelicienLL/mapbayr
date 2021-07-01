test_that("multiplication works", {
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
  mod0 <- mcode("mod0", code0)
  mod0 %>%
    as.list()

  as.list(mod0)$param

  all(as.list(house())$pars == names(as.list(house())$param))
  all(as.list(mod0)$pars == names(as.list(mod0)$param))


  code2 <- "
$PROB Demo MAP BAY
$PARAM TVKA = .5, TVCL = 2, TVV = 100, ETA1 = 0, ETA2 = 0, ETA3 = 0
$PARAM @covariates
BW = 90
$OMEGA 0.1 0.2 0.15
$SIGMA .05 .1
$CMT DEPOT
$INIT CENTRAL = 1000
$ERROR double DV = (CENTRAL / V) * (1 + EPS(1)) ;
$PK
double KA = TVKA * exp(ETA(1) + ETA1);
double CL = TVCL * exp(ETA(2) + ETA2);
double V  = TVV  * exp(ETA(3) + ETA3);
$CAPTURE DV
$PKMODEL ncmt = 1, depot = TRUE
"
  mod2 <- mcode("mod2", code2)
  mod0 %>%
    as.list()


  see(modlib('popex'))


})
