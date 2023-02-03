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

test_that("lloq, LLOQ, and BLQ are checked", {
  dat <- adm_rows(amt = 100, cmt = 1) %>%
    obs_rows(time = 0, cmt = 1, DV = c(0.1, 100), LLOQ = 1, BLQ = c(1L,0L))
  dat[1, c("LLOQ", "BLQ")] <- c(NA_real_)
  dat <- relocate(dat, c("LLOQ", "BLQ"), .after = "DV")

  expect_equal(check_mapbayr_data(dat), dat)

  expect_error(
    check_mapbayr_data(select(dat, -"LLOQ", -"BLQ"), lloq = c(1,2,3)),
    "\"lloq\" must be a single numeric value."
  )

  expect_warning(
    check_mapbayr_data(dat, lloq = 1),
    "LLOQ variable found in data: argument passed to `mapbayest\\(lloq = \\)` will be ignored"
  )

  expect_equal(
    check_mapbayr_data(select(dat, -"LLOQ", -"BLQ"), lloq = 1),
    dat
  )

  expect_error(
    check_mapbayr_data(mutate(dat, LLOQ = c(NA, NA, 1))),
    "Missing values of LLOQ found at an observation record"
  )

  expect_equal(
    check_mapbayr_data(select(dat, -"BLQ")),
    dat
  )

  expect_warning(
    check_mapbayr_data(select(dat, -"LLOQ")),
    "BLQ variable found in the data, but not LLOQ."
  )

  expect_error(
    check_mapbayr_data(mutate(dat, BLQ = c(NA, NA, 0))),
    "Missing values of BLQ found at an observation record"
  )

  expect_error(
    check_mapbayr_data(mutate(dat, BLQ = c(NA, 2, 0))),
    "BLQ values in the data not all equal to 0 or 1"
  )
})
