test_that("stop if no data", {
  mod <- mread("ex_mbr1", mbrlib())
  expect_error(mbrest(mod), "No data provided")
})
