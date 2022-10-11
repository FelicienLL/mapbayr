mod <- exmodel(add_exdata = FALSE, compile = FALSE) %>%
  adm_lines(amt = 1000) %>%
  obs_lines(time = 24, DV = 123)

test_that("covariates can be implicitly and explicitly be called", {

  expected_cov <- as_tibble(data.frame(BW = rep(123, 2), SEX = 0))

  expect_equal(get_data(add_covariates(mod, BW = 123, SEX = 0))[, c("BW", "SEX")], expected_cov)
  expect_equal(get_data(add_covariates(mod, BW = 123))[, "BW"], expected_cov[, "BW"])

  expect_warning(dat1 <- get_data(add_covariates(mod, list(BW = 123, SEX = 0)))[, c("BW", "SEX")], "A list was passed as first argument to ")
  expect_equal(dat1, expected_cov)

  expect_equal(get_data(add_covariates(mod, covariates = list(BW = 123, SEX = 0)))[, c("BW", "SEX")], expected_cov)

  expect_warning(dat2 <- get_data(add_covariates(mod, list(BW = 123), SEX = 0))[, "BW"], "A list was passed as first argument to ")
  expect_equal(dat2, expected_cov[, "BW"])

  expect_equal(get_data(add_covariates(mod, SEX = 0, list(BW = 123)))[, "SEX"], expected_cov[, "SEX"])

  expect_error(get_data(add_covariates(mod, 123, 0)), "Arguments must be named \\(with covariates names\\)")
})

test_that("works with empty arguments", {
   expect_equal(get_data(add_covariates(mod)), get_data(mod))
})
