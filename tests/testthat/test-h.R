test_that("h matrix computation works", {
  expectmat <- matrix(c(400, 0, 200, 0, 1, 0, 1, 0, 0, 40, 0, 20, 0, 1, 0, 1), ncol = 4, nrow = 4)

  expect_equal(h(pred = c(400, 40, 200, 20), cmt = c(2, 3, 2, 3), all_cmt = c(2, 3)), expectmat)
})
