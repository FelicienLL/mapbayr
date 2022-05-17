test_that("hessian = FALSE works", {
  est0 <- mapbayest(exmodel(1, ID = 2), hessian = FALSE)
  expect_true(is.na(est0$covariance))
})

test_that("covariance matrix is correct", {
  expect_false(any(is.na(est001$covariance)))

 # est1b <- mapbayest(model1, data1, verbose = FALSE, hessian = "nlmixrHess")

  nmphi <- matrix(c(1.28120118E-002, 5.40868557E-003, 4.69547364E-004,
                    5.40868557E-003, 2.31664035E-002, 2.19133609E-002,
                    4.69547364E-004, 2.19133609E-002, 1.25252672E-001), nrow = 3, ncol = 3)

  expect_equal(est001$covariance[[1]], nmphi, tolerance = 0.03)
})

test_that("get_cov method is correct", {
  expect_equal(get_cov(est001),
               list(`1` = est001$covariance[[1]],
                    `2` = est001$covariance[[2]],
                    `3` = est001$covariance[[3]],
                    `4` = est001$covariance[[4]],
                    `5` = est001$covariance[[5]],
                    `6` = est001$covariance[[6]],
                    `7` = est001$covariance[[7]],
                    `8` = est001$covariance[[8]]))
  expect_equal(get_cov(mapbayest(exmodel()), simplify = FALSE), list(`1` = est001$covariance[[1]]))
})

test_that("get_phi works", {
  expect_named(get_phi(est001), c("SUBJECT_NO", "ID", "ETA1", "ETA2", "ETA3", "ETC1_1", "ETC2_1", "ETC2_2", "ETC3_1", "ETC3_2", "ETC3_3", "OBJ"))
})
