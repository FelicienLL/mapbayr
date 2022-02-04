test_that("newuoa vs nm", {
  mod <- mread('ex_mbr1', mbrlib())
  data1 <- mod %>%
    adm_lines(amt = 10, addl = 2, ii = 12) %>%
    obs_lines(DV = c(.1, .2), time = c(18, 40)) %>%
    add_covariates(list(WT = 70)) %>%
    get_data()
  est1_n <- mapbayest(mod, data = data1, method = "newuoa", verbose = F)
  est1_l <- mapbayest(mod, data = data1, method = "L-BFGS-B", verbose = F)
  expect_equal(est1_n$final_eta[[1]], est1_l$final_eta[[1]], tolerance = 0.00001)

  data2 <- mod %>%
    adm_lines(amt = 10, addl = 2, ii = 12) %>%
    obs_lines(DV = c(100, 200), time = c(18, 40)) %>%
    add_covariates(list(WT = 70)) %>%
    get_data()
  est2_n <- mapbayest(mod, data = data2, method = "newuoa", verbose = F)
  est2_l <- mapbayest(mod, data = data2, method = "L-BFGS-B", verbose = F, reset = F)
  expect_false(isTRUE(all.equal(est2_n$final_eta[[1]], est2_l$final_eta[[1]], tolerance = 0.1)))
  expect_equal(unname(est2_l$final_eta[[1]][2:3]), est2_l$arg.optim$lower[2:3], tolerance = 0.001)
})
