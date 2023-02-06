library(mapbayr)
mod <- exmodel(301, add_exdata = F, compile = FALSE)
dat <- exdata(301, ID = 1)
test_that("post_mapbay_tab works if missing covariates", {
  dat$SEX <- NULL
  dat$BW <- NULL
  etamat <- matrix(c(0.1, -0.2, 0.3), nrow = 1, dimnames = list("1", c("ETA1", "ETA2", "ETA3")))
  tab <- post_mapbay_tab(x = mod, data = dat, etamat = etamat)
  expect_equal(unique(tab$SEX), 0)
  expect_equal(unique(tab$BW), 75)
})
