est001test <- mapbayest(exmodel(ID = 1:8), verbose = FALSE, progress = FALSE)

test_that("newuoa vs nm", {
  mod <- exmodel(ID = 1)
  est_n <- mapbayest(mod, method = "newuoa")
  expect_equal(get_eta(est_n), get_eta(est001test, output = "list")[[1]], tolerance = 0.00001)

  data2 <- mod %>%
    get_data() %>%
    mutate(DV = c(NA, 300, 300, 300, 300))

  est2_n <- mapbayest(mod, data = data2, method = "newuoa")
  est2_l <- mapbayest(mod, data = data2, method = "L-BFGS-B", reset = F)

  expect_false(isTRUE(all.equal(get_eta(est2_n), get_eta(est2_l), tolerance = 0.1)))
  expect_equal(unname(get_eta(est2_l, 1)), est2_l$arg.optim$lower[1], tolerance = 0.001)
})
