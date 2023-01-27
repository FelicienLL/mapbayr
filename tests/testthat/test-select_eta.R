test_that("select_eta argument works", {
  mod <- mcode("mod",
               "$PARAM ETA1 = 0, ETA2 = 0, ETA3 = 0
               $OMEGA 0.1 0.2 0.3",
               compile = FALSE, cache = FALSE)

  expect_equal(preprocess.optim(mod)$select_eta, c(1,2,3))
  expect_equal(preprocess.optim(mod, select_eta = c(1, 3))$select_eta, c(1,3))
  expect_error(preprocess.optim(mod, select_eta = 4),
               "Cannot select ETA4: maximum 3 ETAs defined in \\$PARAM")

  # works if OMEGA is zero
  mod <- omat(mod, diag(c(0.1,0,0.3)))
  expect_equal(preprocess.optim(mod)$select_eta, c(1,3))
  expect_equal(preprocess.optim(mod, select_eta = c(1))$select_eta, c(1))
  expect_error(preprocess.optim(mod, select_eta = c(1,2,3)),
               "Cannot select ETA2: the corresponding OMEGA value is equal to zero. Modify the \\$OMEGA block or use `mapbayest\\(select_eta = ...\\)`")

})

test_that("estimation and methods works", {
  mod <- exmodel()
  est <- mapbayest(mod, select_eta = c(1,3))

  expect_equal(get_eta(est, 2), c(ETA2 = 0))
  expect_equal(get_cov(est)[,2], c(0,0,0))
  expect_equal(get_cov(est)[2,], c(0,0,0))
  phi <- get_phi(est)
  expect_equal(phi$ETA2, 0)
  expect_equal(phi$ETC2_1, 0)
  expect_equal(phi$ETC2_2, 0)
  expect_equal(phi$ETC3_2, 0)
  expect_equal(param(use_posterior(est))$ETA2, 0)
  histo <- hist(est)
  expect_s3_class(histo, "ggplot")
})

test_that("estimation and methods works", {
  mod <- exmodel() %>% omat(diag(c(0.1, 0, 0.3)))
  est <- mapbayest(mod)

  expect_equal(get_eta(est, 2), c(ETA2 = 0))
  expect_equal(get_cov(est)[,2], c(0,0,0))
  expect_equal(get_cov(est)[2,], c(0,0,0))
  phi <- get_phi(est)
  expect_equal(phi$ETA2, 0)
  expect_equal(phi$ETC2_1, 0)
  expect_equal(phi$ETC2_2, 0)
  expect_equal(phi$ETC3_2, 0)
  expect_equal(param(use_posterior(est))$ETA2, 0)
  histo <- hist(est)
  expect_s3_class(histo, "ggplot")

  # reset is ok

  expect_message(mapbayest(mod, quantile_bound = 0.2), "Reset with new bounds")

  argofv <- c(
    preprocess.ofv.fix(x = mod, data = get_data(mod), select_eta = c(1,3)),
    preprocess.ofv.id(x = mod, iddata = get_data(mod))
  )

  new_inits <- new_ini3(arg.ofv = argofv, upper = c(1,2), nreset = 1, select_eta = c(1,3))
  expect_type(new_inits, "double")
  expect_length(new_inits, 2)
  expect_named(new_inits, c("ETA1", "ETA3"))
})
