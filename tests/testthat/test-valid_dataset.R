dat <- exdata()
test_that("NM-TRAN items are specified", {
  expect_error(check_mapbayr_data(select(dat, -ID, -time)), "Missing column: ID time")
  expect_error(check_mapbayr_data(select(dat, -time, -evid, -cmt, -amt)), "Missing column: time evid cmt amt")
})

test_that("stops if non-numeric columns", {
  expect_error(check_mapbayr_data(mutate(dat, hello = "world", foo = "bar")), "Non-numeric column found: hello foo")
})

test_that("auto-supply of MDV if missing", {
  data_mdv <- select(dat, -mdv)
  datachecked <- check_mapbayr_data(data_mdv)

  expect_true(!is.null(datachecked[["mdv"]]))
  expect_equal(datachecked[["mdv"]], c(1, rep(0,4)))
})

test_that("misspecification of evid/mdv are checked", {
  expect_error(check_mapbayr_data(dat %>% mutate(evid = c(1, 0, 0, 2, 0))), "Lines with evid = 2 & mdv = 0 are not allowed")
  expect_error(check_mapbayr_data(dat %>% mutate(evid = c(1, 0, 0, 1, 0))), "Lines with mdv = 0 must have evid = 0.")
  dat[2, "time"] <- 0
  expect_error(check_mapbayr_data(dat), "Observation line \\(mdv = 0\\) not accepted at time = 0")
})


