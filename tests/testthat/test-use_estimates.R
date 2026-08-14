est001 <- mapbayest(exmodel(1, ID = 1:4))
est301 <- mapbayest(exmodel(301, ID = 1:4))
test_that("use_estimates() works", {
  expect_s4_class(use_estimates(est001), "mrgmod")
  expect_s4_class(use_estimates(est301), "mrgmod")
})

test_that("use_estimates(use_eta) works", {
  ans <- use_estimates(est301)@args$idata
  expect_equal(ans$ID, as.double(1:4))
  expect_true(all(c("ETA1", "ETA2", "ETA3") %in% colnames(ans)))
  expect_true(all(c(ans$ETA1, ans$ETA2, ans$ETA3) != 0))

  ans2 <- use_estimates(est301, use_eta = TRUE)@args$idata
  expect_equal(ans2, ans)

  ans3 <- use_estimates(est301, use_eta = FALSE)@args$idata
  expect_true(all(!c("ETA1", "ETA2", "ETA3") %in% colnames(ans3)))
})

test_that("use_estimates(use_covariates) works", {
  ans <- use_estimates(est301)@args$idata
  expect_true(all(c("BW", "SEX") %in% colnames(ans)))

  ans2 <- use_estimates(est301, use_covariates = TRUE)@args$idata
  expect_equal(ans2, ans)

  ans3 <- use_estimates(est301, use_covariates = FALSE)@args$idata
  expect_true(all(!c("BW", "SEX") %in% colnames(ans3)))

  ans4 <- use_estimates(est301, use_eta = FALSE, use_covariates = FALSE)@args$idata
  expect_null(ans4)

  dat301_missingSEX <- exdata(301, ID = 1:4)
  dat301_missingSEX$SEX <- NULL

  est301missingSEX <- mapbayest(
    exmodel(301, add_exdata = FALSE),
    data = dat301_missingSEX,
    verbose = FALSE
  )

  expect_named(
    use_estimates(est301missingSEX)@args$idata,
    c("ID", "ETA1", "ETA2", "ETA3", "BW")
  )
})


test_that("use_estimates(.etasrc) works", {
  ans <- use_estimates(est301)@args$etasrc
  expect_equal(ans, "idata.all")

  ans <- use_estimates(est301, .etasrc = "data")@args$etasrc
  expect_equal(ans, "data")

  ans <- use_estimates(est301, .etasrc = NULL)@args$etasrc
  expect_null(ans)
})

test_that("use_estimates(.etasrc) works", {
  expect_equal(
    omat(use_estimates(est001), make = TRUE),
    matrix(rep(0, 9), nrow = 3)
  )
  expect_equal(
    smat(use_estimates(est001), make = TRUE),
    matrix(rep(0, 4), nrow = 2)
  )

  mod001 <- exmodel(add_exdata = FALSE, compile = FALSE, quiet = TRUE)

  expect_equal(
    omat(use_estimates(est001, .zero_re = "omega"), make = TRUE),
    matrix(rep(0, 9), nrow = 3)
  )
  expect_equal(
    smat(use_estimates(est001, .zero_re = "omega"), make = TRUE),
    smat(mod001, make = TRUE)
  )

  expect_equal(
    omat(use_estimates(est001, .zero_re = "sigma"), make = TRUE),
    omat(mod001, make = TRUE)
  )
  expect_equal(
    smat(use_estimates(est001, .zero_re = "sigma"), make = TRUE),
    matrix(rep(0, 4), nrow = 2)
  )

  expect_equal(
    omat(use_estimates(est001, .zero_re = "none"), make = TRUE),
    omat(mod001, make = TRUE)
  )
  expect_equal(
    smat(use_estimates(est001, .zero_re = "none"), make = TRUE),
    smat(mod001, make = TRUE)
  )
})

test_that("use_estimates(verbose) works", {
  expect_message(use_estimates(est001, verbose = TRUE))
  expect_no_message(use_estimates(est001, verbose = FALSE))
})


