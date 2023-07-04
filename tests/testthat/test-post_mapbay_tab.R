library(mapbayr)
mod301 <- exmodel(301, add_exdata = F, cache = TRUE)
dat <- exdata(301, ID = 1)
etamat <- matrix(c(0.1, -0.2, 0.3), nrow = 1, dimnames = list("1", c("ETA1", "ETA2", "ETA3")))
test_that("post_mapbay_tab works if missing covariates", {
  dat$SEX <- NULL
  dat$BW <- NULL
  tab <- post_mapbay_tab(x = mod301, data = dat, etamat = etamat)
  expect_equal(unique(tab$SEX), 0)
  expect_equal(unique(tab$BW), 75)
})

test_that("correct number of rows if no observation rows", {
  expect_equal(
    nrow(post_mapbay_tab(mod301, dat[1,], etamat = etamat)),
    1
  )
})
