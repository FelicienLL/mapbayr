test_that("mbrest verbose works", {
  mod <- mread('ex_mbr1', mbrlib())
  data <- mod %>%
    adm_lines(amt = 10, addl = 2, ii = 12) %>%
    obs_lines(DV = c(.1, .2), time = c(18, 40)) %>%
    see_data()

  expect_output(mbrest(mod, data))
  expect_output(mbrest(mod, data, verbose = T))
  expect_output(mbrest(mod, data, verbose = F), regexp = NA)
})
