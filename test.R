library(tidyverse)
library(mrgsolve)
datacabo <- bind_rows(
  tibble(ID = 1, time = 0, amt = 40, addl = 9, ii = 24, rate = c(0, -2), cmt = c(1, 2), evid = 1, mdv = 1),
  tibble(ID = 1, time = 240+22, evid = 0, mdv = 0, cmt = 2, DV = 0.890, rate = 0)
)
modelcabo <- load_mapbay_model("cabozantinib_nguyen")

dataibru <- bind_rows(
  tibble(ID = 1, time = 0, amt = 420000, addl = 10, ii = 24, rate = -2, cmt = 1, evid = 1, mdv = 1),
  tibble(ID = 1, time = 240-1, evid = 0, mdv = 0, cmt = c(2,4), DV = c(6, 25), rate = 0),
  tibble(ID = 1, time = 240+2, evid = 0, mdv = 0, cmt = c(2,4), DV = c(200, 130), rate = 0),
  tibble(ID = 1, time = 240+4, evid = 0, mdv = 0, cmt = c(2,4), DV = c(60, 100), rate = 0)
)
dataibru
modelibru <- load_mapbay_model("ibrutinib_gallais")
print_code(modelibru)

devtools::load_all()
mapbay_estimation(data = datacabo, model= modelcabo)
mapbay_estimation(data = dataibru, model= modelibru)

modelibru$mrgsolve_model@cmtL


mapbay_estimation(data = datacabo, model= modelcabo ,output_df = T) %>%
  select(-any_of(modelcabo$mrgsolve_model@cmtL))

