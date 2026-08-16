# tests/testthat/test-h.R

test_that("h processes a standard two-compartment RUVDEF correctly", {
  # Define the RUVDEF matrix as used in the original example
  RUVDEF <- matrix(c(1, 2, 3, 4), nrow = 2, dimnames = list(NULL, c("2", "3")))

  pred <- c(500, 400, 40, 100, 200, 20)
  cmt  <- c(2, 2, 3, 3, 2, 3)

  res <- h(pred, cmt, RUVDEF)

  # Expected output calculated manually:
  # cmt 2 -> column 1 (pred) and column 2 (1)
  # cmt 3 -> column 3 (pred) and column 4 (1)
  expected <- matrix(
    c(
      500,   1,   0,   0,
      400,   1,   0,   0,
      0,   0,  40,   1,
      0,   0, 100,   1,
      200,   1,   0,   0,
      0,   0,  20,   1
    ),
    nrow = 6,
    byrow = TRUE
  )

  expect_equal(res, expected)
})

test_that("h processes a concentration of 0", {
  # Define the RUVDEF matrix as used in the original example
  RUVDEF <- matrix(c(1, 2, 3, 4), nrow = 2, dimnames = list(NULL, c("2", "3")))

  pred <- c(0, 400, 40, 100, 200, 20)
  cmt  <- c(2, 2, 3, 3, 2, 3)

  res <- h(pred, cmt, RUVDEF)

  # Expected output calculated manually:
  # cmt 2 -> column 1 (pred) and column 2 (1)
  # cmt 3 -> column 3 (pred) and column 4 (1)
  expected <- matrix(
    c(
      1,   1,   0,   0,
      400,   1,   0,   0,
      0,   0,  40,   1,
      0,   0, 100,   1,
      200,   1,   0,   0,
      0,   0,  20,   1
    ),
    nrow = 6,
    byrow = TRUE
  )

  expect_equal(res, expected)
})

test_that("h correctly ignores NA in the additive (second row) position", {
  ruvdef_na_add <- matrix(c(1, NA), nrow = 2, dimnames = list(NULL, c("2")))

  pred <- c(5000, 200, 20)
  cmt  <- c(2, 2, 2)

  res <- h(pred, cmt, ruvdef_na_add)

  # The maximum index in ruvdef is 1, so it should return a 3x1 matrix
  # It should contain only the predictions, no 1s
  expected <- matrix(c(5000, 200, 20), nrow = 3, ncol = 1)

  expect_equal(res, expected)
})

test_that("h correctly ignores NA in the proportional (first row) position", {
  ruvdef_na_prop <- matrix(c(NA, 1), nrow = 2, dimnames = list(NULL, c("2")))

  pred <- c(5000, 200, 20)
  cmt  <- c(2, 2, 2)

  res <- h(pred, cmt, ruvdef_na_prop)

  # The maximum index in ruvdef is 1, so it should return a 3x1 matrix
  # It should contain only 1s, ignoring the prediction values
  expected <- matrix(c(1, 1, 1), nrow = 3, ncol = 1)

  expect_equal(res, expected)
})

test_that("h handles length-1 inputs correctly", {
  # Edge case: A single row dataset
  ruvdef <- matrix(c(1, 2), nrow = 2, dimnames = list(NULL, c("2")))

  pred <- 500
  cmt  <- 2

  res <- h(pred, cmt, ruvdef)

  # Should return a 1x2 matrix
  expected <- matrix(c(500, 1), nrow = 1, ncol = 2)

  expect_equal(res, expected)
})

test_that("h handles non-sequential target columns in RUVDEF", {
  # Edge case: The target columns have gaps (e.g., maps to columns 1 and 5)
  # This tests if n_cols <- max(ruvdef) correctly sizes and pads the matrix with 0s
  ruvdef <- matrix(c(1, 5), nrow = 2, dimnames = list(NULL, c("2")))

  pred <- c(10, 20)
  cmt  <- c(2, 2)

  res <- h(pred, cmt, ruvdef)

  # Expected: 2x5 matrix. Columns 2, 3, and 4 should be completely zero.
  expected <- matrix(
    c(
      10, 0, 0, 0, 1,
      20, 0, 0, 0, 1
    ),
    nrow = 2,
    byrow = TRUE
  )

  expect_equal(res, expected)
})

test_that("h safely ignores cmt values not present in RUVDEF", {
  # Edge case: The dataset contains a cmt (e.g., 4) that is not defined in RUVDEF
  ruvdef <- matrix(c(1, 2), nrow = 2, dimnames = list(NULL, c("2")))

  # Row 2 has cmt = 4, which is unknown
  pred <- c(100, 200, 300)
  cmt  <- c(2, 4, 2)

  res <- h(pred, cmt, ruvdef)

  # For cmt = 4, match() returns NA, which means c_prop and c_add become NA.
  # The row should default to all 0s without throwing an error.
  expected <- matrix(
    c(
      100, 1,
      0, 0,  # Unmatched cmt gets left as zeros
      300, 1
    ),
    nrow = 3,
    byrow = TRUE
  )

  expect_equal(res, expected)
})

test_that("h handles RUVDEF columns in arbitrary order", {
  # Edge case: The columns in RUVDEF are not sorted numerically
  ruvdef <- matrix(
    c(3, 4, 1, 2),
    nrow = 2,
    dimnames = list(NULL, c("3", "2")) # cmt 3 comes before cmt 2 in the definition
  )

  pred <- c(50, 60)
  cmt  <- c(2, 3)

  res <- h(pred, cmt, ruvdef)

  # cmt = 2 should still map to cols 1 and 2
  # cmt = 3 should still map to cols 3 and 4
  expected <- matrix(
    c(
      50, 1,  0, 0,
      0, 0, 60, 1
    ),
    nrow = 2,
    byrow = TRUE
  )

  expect_equal(res, expected)
})
