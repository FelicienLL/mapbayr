test_that("evid = 2 are handled correctly", {
  dat <- mutate(exdata(), evid = c(1, 0, 0, 0, 2))
  expect_error(check_mapbayr_data(dat), "Lines with evid = 2 & mdv = 0 are not allowed")

  dat[5, "mdv"] <- 1
  expect_error(check_mapbayr_data(dat), NA)
})
