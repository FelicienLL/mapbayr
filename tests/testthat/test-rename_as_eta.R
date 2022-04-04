test_that("rename_as_eta works", {
  expect_equal(rename_as_eta(c(0.1, 0.2, 0.3)), c(ETA1 = 0.1, ETA2 = 0.2, ETA3 = 0.3))
  mat <- do.call(cbind, list(ETA1 = c(0.1, 1), ETA2 = c(0.2, 2), ETA3 = c(0.3, 3)))
  expect_equal(rename_as_eta(matrix(c(0.1, 1, 0.2, 2, 0.3, 3), ncol = 3)), mat)
})
