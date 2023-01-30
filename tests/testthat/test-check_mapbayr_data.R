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
})

test_that("DV cannot be NA if mdv == 0", {
  dat[3, "DV"] <- NA
  expect_error(check_mapbayr_data(dat), "DV cannot be missing \\(NA\\) on an observation line \\(mdv = 0\\)")
})

test_that(".datehour columns are removed", {
  dat <- adm_rows(cmt = 1, amt = 100, .datehour = "2022/01/01 12:34:56") %>%
    obs_rows(cmt = 2) %>%
    as.data.frame()
  expect_equal(dat[1,".datehour"], as.POSIXct("2022/01/01 12:34:56", tz = "UTC"))
  expect_null(check_mapbayr_data(dat)[[".datehour"]])
})
