library(testthat)
library(mapbayr)

test_that("compute derivatives", {
  mbr_der <- derivatives(
    v_DV = c(400, 40, 200, 20),
    v_cmt = c(2, 3, 2, 3),
    cmts = c(2,3)
  )
  a <- matrix(c(400, 0, 200, 0, 1, 0, 1, 0, 0, 40, 0, 20, 0, 1, 0, 1), ncol = 4)
  colnames(a) <- c("P2", 'A2', "P3", "A3")
  expect_equal(mbr_der, a)
})
