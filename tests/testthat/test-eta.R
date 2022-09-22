modeta <- exmodel(compile = FALSE)
test_that("eta works", {
  expect_equal(eta(modeta), c(ETA1 = 0, ETA2 = 0, ETA3 = 0))
  expect_equal(eta(mrgsolve::house()), NULL)
  expect_equal(eta(0.1), c(ETA1 = 0.1))
  expect_equal(eta(1, 0.2, c(0.3, 0.4)), c(ETA1 = 1, ETA2 = 0.2, ETA3 = 0.3, ETA4 = 0.4))
  expect_equal(eta(1, 0.2, list(0.3, 0.4)), c(ETA1 = 1, ETA2 = 0.2, ETA3 = 0.3, ETA4 = 0.4))
  expect_equal(eta(), NULL)
  expect_equal(eta(n = 3), c(ETA1 = 0, ETA2 = 0, ETA3 = 0))
  expect_equal(eta(n = 3, val = 1), c(ETA1 = 1, ETA2 = 1, ETA3 = 1))
})

test_that("rename_as_eta works", {
  expect_equal(rename_as_eta(c(0.1, 0.2, 0.3)), c(ETA1 = 0.1, ETA2 = 0.2, ETA3 = 0.3))
  mat <- do.call(cbind, list(ETA1 = c(0.1, 1), ETA2 = c(0.2, 2), ETA3 = c(0.3, 3)))
  expect_equal(rename_as_eta(matrix(c(0.1, 1, 0.2, 2, 0.3, 3), ncol = 3)), mat)
})

test_that("eta_length works", {
  expect_equal(eta_length(modeta), 3)
  expect_equal(eta_length(c(ETA1 = 0, ETA2 = 0)), 2)
})

test_that("eta_names works", {
  expect_equal(eta_names(modeta), c("ETA1", "ETA2", "ETA3"))
  expect_equal(eta_names(c(ETA1 = 0, ETA2 = 0)), c("ETA1", "ETA2"))
})
