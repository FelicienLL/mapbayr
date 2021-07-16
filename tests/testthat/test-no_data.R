test_that("stop if no data", {
  mod <- mread("ex_mbr1", mbrlib())
  expect_error(mapbayest(mod), "No data provided")
})

test_that("don't stop if no observation", {
  mod3 <- mread("ex_mbr3", mbrlib())
  dat3 <- mod3 %>%
    adm_lines(amt = 100) %>%
    get_data()
  dat3$DV <- NA_real_
  expect_error(est3 <- suppressWarnings(mapbayest(mod3, dat3, verbose = F)), NA)
  expect_equal(get_eta(est3), c(ETA1 = 0, ETA2 = 0, ETA3 = 0), tolerance = 10^-6)

  dat4 <- mod3 %>%
    adm_lines(amt = 100) %>%
    obs_lines(time = 10, DV  = 1.1, mdv = 1) %>%
    get_data()

  expect_error(est4 <- suppressWarnings(mapbayest(mod3, dat4, verbose = F)), NA)
  expect_equal(get_eta(est4), c(ETA1 = 0, ETA2 = 0, ETA3 = 0), tolerance = 10^-6)

})
