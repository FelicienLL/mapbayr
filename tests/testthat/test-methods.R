test_that("newuoa vs nm", {
  mod <- mread('ex_mbr1', mbrlib())
  data1 <- mod %>%
    adm_lines(amt = 10, addl = 2, ii = 12) %>%
    obs_lines(DV = c(.1, .2), time = c(18, 40)) %>%
    add_covariates(list(WT = 70)) %>%
    see_data()
  est1_n <- mod %>%
    data_set(data1) %>%
    mbrest(method = "newuoa")
  est1_l <- mod %>%
    data_set(data1) %>%
    mbrest(method = "L-BFGS-B")
  expect_equal(est1_n$final_eta, est1_l$final_eta, tolerance = 0.00001)

  data2 <- mod %>%
    adm_lines(amt = 10, addl = 2, ii = 12) %>%
    obs_lines(DV = c(100, 200), time = c(18, 40)) %>%
    add_covariates(list(WT = 70)) %>%
    see_data()
  est2_n <- mod %>%
    data_set(data2) %>%
    mbrest(method = "newuoa")
  est2_l <- mod %>%
    data_set(data2) %>%
    mbrest(method = "L-BFGS-B")
  expect_false(isTRUE(all.equal(est2_n$final_eta, est2_l$final_eta, tolerance = 0.1)))
  expect_equal(unname(est2_l$final_eta[2:3]), est2_l$arg.optim$lower[2:3], tolerance = 0.001)
})
