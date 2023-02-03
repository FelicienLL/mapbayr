test_that("fill_eta works", {
  expect_equal(
    fill_eta(c(ETA1 = 1, ETA3 = 0.33), n = 4),
    c(ETA1 = 1, ETA2 = 0, ETA3 = 0.33, ETA4 = 0)
  )

  expect_error(
    fill_eta(eta(n=3)),
    "n is missing"
  )

  mat <- matrix(1:8, ncol = 2)
  colnames(mat) <- c("ETA2", "ETA3")
  expect_equal(
    fill_eta(mat, n = 4),
    matrix(c(0, 0, 0, 0,
             1, 2, 3, 4,
             5, 6, 7, 8,
             0, 0, 0, 0), nrow = 4, ncol = 4, dimnames = list(NULL, c("ETA1", "ETA2", "ETA3", "ETA4")))
  )
})
