dat <- adm_rows(amt = 1000, cmt = 1) %>%
  obs_rows(time = 24, DV = 123, cmt = 1)

test_that("covariates can be called through `...` or `covariates = list()`", {
  expected_cov <- as_tibble(data.frame(BW = rep(123, 2), SEX = 0))

  expect_equal(add_covariates(dat, BW = 123, SEX = 0)[, c("BW", "SEX")], expected_cov)
  expect_equal(add_covariates(dat, BW = 123)[, "BW"], expected_cov[, "BW"])

  expect_warning(dat1 <- add_covariates(dat, list(BW = 123, SEX = 0))[, c("BW", "SEX")], "A list was passed as first argument to ")
  expect_equal(dat1, expected_cov)

  expect_equal(add_covariates(dat, covariates = list(BW = 123, SEX = 0))[, c("BW", "SEX")], expected_cov)

  expect_warning(dat2 <- add_covariates(dat, list(BW = 123), SEX = 0)[, "BW"], "A list was passed as first argument to ")
  expect_equal(dat2, expected_cov[, "BW"])

  expect_equal(add_covariates(dat, SEX = 0, list(BW = 123))[, "SEX"], expected_cov[, "SEX"])

  expect_error(add_covariates(dat, 123, 0), "Arguments must be named \\(with covariates names\\)")
})

test_that("works with empty arguments", {
   expect_equal(add_covariates(dat), dat)
})

test_that("AOLA TOLA works", {
  expect_equal(add_covariates(dat, AOLA = TRUE, TOLA = TRUE)[,c("AOLA", "TOLA")], tibble::tibble(AOLA = c(1000, 1000), TOLA = c(0,0)))

  #updates afterwards
  dat2 <- dat %>%
    add_covariates(AOLA = TRUE, TOLA = TRUE) %>%
    adm_rows(time = 48, cmt = 1, amt = 2000)

  expect_equal(dat2[,c("AOLA", "TOLA")], tibble::tibble(AOLA = c(1000, 1000, 2000), TOLA = c(0,0, 48)))

  model <- mcode("model", "
  $PARAM @annotated @covariates
  TOLA : 0 : Time Last Adm
  AOLA : 0 : Amount Last Adm
  ", compile = FALSE)

  #automatic if AOLA/TOLA are cov
  dat3 <- model %>%
    adm_rows(time = c(0, 24, 48), amt = c(100, 200, 300), cmt = 1) %>%
    add_covariates() %>%
    get_data()

  expect_equal(dat3$AOLA, c(100, 200, 300))
  expect_equal(dat3$TOLA, c(0, 24, 48))

})

test_that("cannot have '.datehour'",{
  dat <- adm_rows(amt = 100, cmt = 1)
  expect_error(add_covariates(dat, .datehour = "123"), "Cannot have a covariate named: .datehour")
  expect_error(add_covariates(dat, covariates = list(.datehour = "123")), "Cannot have a covariate named: .datehour")
})
