test_that("qparam works", {
  ho <- mrgsolve::house()

  expectparam <- param(ho, c(CL = .1, VC = 1))

  expect_equal(
    qparam(ho, c(CL = .1, VC = 1)),
    expectparam
  )

  expect_equal(
    qparam(ho, list(CL = .1, VC = 1)),
    expectparam
  )

  #do not work is passed as matrix
  p_matrix <- matrix(c(0.1, 1), ncol = 2, dimnames = list(NULL, c("CL", "VC")))

  expect_false(
    qparam(ho, p = p_matrix)$CL == .1
  )

})
