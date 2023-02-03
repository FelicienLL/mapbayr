test_that("use_posterior obeys to update_x arguments", {
  mod301 <- exmodel(301)
  est301 <- mapbayest(mod301)
  defaultposterior <- use_posterior(est301, .zero_re = "none")

  expect_equal(defaultposterior$ETA1, 0.397629671)
  expect_equal(defaultposterior$BW, 77)
  defaultposterior %>%
    omat(make = TRUE) %>%
    expect_equal(diag(c(0.2, 0.2, 0.2)))

  est301 %>%
    use_posterior(update_omega = FALSE, .zero_re = "none") %>%
    omat(make = TRUE) %>%
    expect_equal(diag(c(0.2, 0.2, 0.2)))

  est301 %>%
    use_posterior(update_omega = TRUE, .zero_re = "none") %>%
    omat(make = TRUE) %>%
    expect_equal(matrix(c(0.010656961, 0.01131633, 0.003616018,
                          0.011316334, 0.02123521, 0.016801093,
                          0.003616018, 0.01680109, 0.129059974), nrow = 3), tolerance = 1e-6)

  expect_equal(use_posterior(est301, update_cov = FALSE)$BW, 75)
  expect_equal(use_posterior(est301, update_cov = TRUE)$BW, 77)

  expect_equal(use_posterior(est301, update_eta = FALSE)$ETA1, 0)
  expect_equal(use_posterior(est301, update_eta = TRUE)$ETA1, 0.397629671)

  dat301_missingSEX <- get_data(mod301)
  dat301_missingSEX$SEX <- NULL

  expect_equal(use_posterior(mapbayest(mod301, dat301_missingSEX))$SEX, 0)

})

test_that("zero_re in use_posterior", {
  est <- mapbayest(exmodel())
  zero_all <- expect_warning(est %>% use_posterior(), NA) #fix 115

  expect_equal(unname(omat(zero_all, make = T)), diag(c(0,0,0)))
  expect_equal(unname(smat(zero_all, make = T)), diag(c(0,0)))


  zero_omega <- est %>%
    use_posterior(.zero_re = "omega")

  expect_equal(unname(omat(zero_omega, make = T)), diag(c(0,0,0)))
  expect_equal(unname(smat(zero_omega, make = T)), diag(c(0.05,0)))


  zero_sigma <- est %>%
    use_posterior(.zero_re = "sigma")

  expect_equal(unname(omat(zero_sigma, make = T)), diag(c(0.2,0.2,0.2)))
  expect_equal(unname(smat(zero_sigma, make = T)), diag(c(0,0)))

  zero_none <- est %>%
    use_posterior(.zero_re = "none")

  expect_equal(unname(omat(zero_none, make = T)), diag(c(0.2,0.2,0.2)))
  expect_equal(unname(smat(zero_none, make = T)), diag(c(0.05,0)))

  zero_covariance <- est %>%
    use_posterior(update_omega = TRUE)

  expect_equal(unname(omat(zero_covariance, make = T)), get_cov(est))
  expect_equal(unname(smat(zero_covariance, make = T)), diag(c(0,0)))

})

test_that("multi ID", {
  post001 <- est001 %>% use_posterior(update_omega = TRUE)

  expect_length(post001, 8)

  expect_equal(omat(post001[[1]], make = TRUE), get_cov(est001)[[1]], tolerance = 1e-6)
  expect_equal(omat(post001[[2]], make = TRUE), get_cov(est001)[[2]], tolerance = 1e-6)

  expect_equal(post001[[1]]$ETA1, 0.40505701)
  expect_equal(post001[[2]]$ETA1, -0.145365840)
})

