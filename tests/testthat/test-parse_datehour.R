test_that("parse_datehour works", {
  options(mapbayr.datehour = NULL)
  dh1 <- as.POSIXct("2022-02-02 22:33:44", tz = "UTC")
  dh2 <- as.POSIXct("2022-02-02 22:33:00", tz = "UTC")

  # POSITct
  expect_equal(parse_datehour(x = dh1), dh1)

  # numeric
  expect_equal(parse_datehour(1643841224), dh1)

  # characters
  expect_equal(parse_datehour(x = "22:33:44 02-02-2022", orders = "HMS dmY"), dh1)

  # default formats
  expect_equal(parse_datehour(x = "2022-02-02 22:33:44"), dh1)
  expect_equal(parse_datehour(x = "2022-02-02 22:33"), dh2)
  expect_equal(parse_datehour(x = "02-02-2022 22:33:44"), dh1)
  expect_equal(parse_datehour(x = "02-02-2022 22:33"), dh2)

  #through options with adm_rows/obs_rows

  expect_equal(adm_rows(.datehour = "22:33 02-02-2022", amt = 100, cmt = 1)$time, NA_real_)
  options(mapbayr.datehour = "HM dmY")
  expect_equal(
    adm_rows(.datehour = "22:33 02-02-2022", amt = 100, cmt = 1),
    tibble::tibble(ID = 1L, time = 0, evid = 1L, cmt = 1, amt = 100, mdv = 1L, .datehour = dh2)
  )
  options(mapbayr.datehour = NULL)
})
