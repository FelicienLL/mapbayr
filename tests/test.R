library(tidyverse)
library(mrgsolve)
datacabo <- bind_rows(
  tibble(ID = 1, time = 0, amt = 60, addl = 9, ii = 24, rate = c(0, -2), cmt = c(1, 2), evid = 1, mdv = 1),
  tibble(ID = 1, time = 240+22, evid = 0, mdv = 0, cmt = 2, DV = 0.890, rate = 0),
  tibble(ID = 1, time = 240+70, evid = 0, mdv = 0, cmt = 2, DV = 0.650, rate = 0)
)
dataibru <- bind_rows(
  tibble(ID = 1, time = 0, amt = 420000, addl = 10, ii = 24, rate = -2, cmt = 1, evid = 1, mdv = 1),
  tibble(ID = 1, time = 240-1, evid = 0, mdv = 0, cmt = c(2,4), DV = c(10, 25), rate = 0),
  tibble(ID = 1, time = 240+2, evid = 0, mdv = 0, cmt = c(2,4), DV = c(160, 200), rate = 0),
  tibble(ID = 1, time = 240+4, evid = 0, mdv = 0, cmt = c(2,4), DV = c(36, 100), rate = 0)
)

dataibru2 <- bind_rows(
  tibble(ID = 1, time = 0, amt = 420000, ss = 1, ii = 24, rate = -2, cmt = 1, evid = 1, mdv = 1),
  tibble(ID = 1, time = 23, evid = 0, mdv = 0, cmt = c(2,4), DV = c(10, 25), rate = 0),
  tibble(ID = 1, time = 24, amt = 420000, ss = 0, ii = 0, rate = -2, cmt = 1, evid = 1, mdv = 1),
  tibble(ID = 1, time = 26, evid = 0, mdv = 0, cmt = c(2,4), DV = c(160, 200), rate = 0),
  tibble(ID = 1, time = 28, evid = 0, mdv = 0, cmt = c(2,4), DV = c(36, 100), rate = 0)
)



mread(system.file("mrg_models", paste0("example_test_mapbay.cpp"), package = "mapbayr"))


devtools::load_all()
modelcabo <- mread("Y:/PK-LE-LOUEDEC-FELICIEN/PACKAGE/mbrapp/inst/mrg_models/cabozantinib_nguyen_mapbay.cpp")
modelibru <- mread("Y:/PK-LE-LOUEDEC-FELICIEN/PACKAGE/mbrapp/inst/mrg_models/ibrutinib_gallais_mapbay.cpp")
modelpazo <- mread("Y:/PK-LE-LOUEDEC-FELICIEN/PACKAGE/mbrapp/inst/mrg_models/pazopanib_yu_mapbay.cpp")

model <- modelcabo
mbr_drug(modelcabo)
mbr_model_ref(modelcabo)
mbr_model_name(modelcabo)
mbr_model_file(modelcabo)
adm_cmt(modelcabo)
obs_cmt(modelibru)
adm_0_cmt(modelcabo)
mbr_conc_unit(modelcabo)
mbr_conc_scaling(modelcabo)
log.transformation(modelcabo)
mbr_cov_names(modelcabo)
mbr_cov_refvalues(modelcabo)
mbr_cov_descr(modelcabo)
mbr_param_names(modelcabo)
mbr_param_units(modelcabo)
mbr_param_tv(modelcabo)

modelcabo %>%
  data_set(datacabo) %>%
  mbrest()


estcabo <- modelcabo %>%
  data_set(datacabo) %>%
  mbrest()


estcabo$model %>%
  omat(make = T) %>%
  diag()

estcabo$final_eta


mbr_cov_names(modelcabo)

devtools::load_all()
modelcabo %>%
  adm_lines(addl = 20, ii = 24, amt = 60) %>%
  param(AOLA = 60, AGE = 80, WT = 55, SEX = 1, RCC = 1) %>%
  obs_lines(DV = .69, time = 20*24+23) %>%
  mbrest() %>%
  mbrpred(delta = 1) %>%
  mbrplot()

modelcabo %>%
  adm_lines(amt = 100) %>%
  obs_lines(time = 12, DV = 1) %>%
  see_data(n = 1)


ex1 <-mread('Y:/PK-LE-LOUEDEC-FELICIEN/PACKAGE/mapbayr/inst/mrg_models/examplinib1_mapbay.cpp')

ex2 <- mread('Y:/PK-LE-LOUEDEC-FELICIEN/PACKAGE/mapbayr/inst/mrg_models/examplinib2_mapbay.cpp')

ex2 %>%
  adm_lines(amt = 1000, addl = 10, ii = 24) %>%
  zero_re() %>%
  mrgsim(end = 300) %>%
  plot_sims(PAR, MET)

ex2 %>%
  adm_lines(amt = 1000, addl = 10, ii = 24, realize_addl = T) %>%
  obs_lines(time = c(250, 264), DV = c(40, 15), DVmet = c(50, 40)) %>%
  mbrest() %>%
  mbrpred(start = 240, delta = .1) %>%
  mbrplot()+
  theme(legend.position = "bottom")


ex1 %>%
  adm_lines(amt = 100) %>%
  obs_lines(DV = .5, time = 25) %>%
  obs_lines(DV = .4, time = 31) %>%
  mbrest() %>%
  mbrpred() %>%
  mbrplot()

ex3 <- mread('Y:/PK-LE-LOUEDEC-FELICIEN/PACKAGE/mapbayr/inst/mrg_models/examplinib3_mapbay.cpp')

ex3 %>%
  adm_lines(amt = 560, rate = 560) %>%
  obs_lines(time = c(.95, 2, 5), DV = c(50, 20, 5)) %>%
  mbrest() %>%
  mbrpred() %>%
  mbrplot()


devtools::load_all()
derivatives()

derivatives(
  v_DV = c(400, 40, 200, 20),
  v_cmt = c(2, 3, 2, 3),
  cmts = c(2,3)
  )

derivatives(
  v_DV = c(400, 40, 200, 20),
  v_cmt = c(2, 3, 2, 3),
  cmts = c(2,3)
)
