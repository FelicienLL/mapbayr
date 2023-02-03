test_that("obs_rows.data.frame works", {
  # Single
  expect_equal(obs_rows(time = 24, cmt = 2, DV = 0.1),
               tibble::tibble(ID = 1L, time = 24, evid = 0L, cmt = 2L, DV = 0.1, mdv = 0L))

  # 1 multiple
  expect_equal(obs_rows(time = c(24, 48), cmt = 2, DV = 0.1),
               tibble::tibble(ID = 1L, time = c(24, 48), evid = 0L, cmt = 2L, DV = 0.1, mdv = 0L))
  expect_equal(obs_rows(time = 24, cmt = c(2, 4), DV = 0.1),
               tibble::tibble(ID = 1L, time = 24, evid = 0L, cmt = c(2L, 4L), DV = 0.1, mdv = 0L))
  expect_equal(obs_rows(time = 24, cmt = 2, DV = c(0.1, 0.2, 0.3)),
               tibble::tibble(ID = 1L, time = 24, evid = 0L, cmt = 2L, DV = c(0.1, 0.2, 0.3), mdv = 0L))

  # 2 multiples.
  # Only should cross time and cmt
  expect_equal(obs_rows(time = c(24, 48), cmt = 2, DV = c(0.1, 100)),
               tibble::tibble(ID = 1L, time = c(24, 48), evid = 0L, cmt = 2L, DV = c(0.1, 100), mdv = 0L))
  expect_equal(obs_rows(time = 24, cmt = c(2, 4), DV = c(0.1, 100)),
               tibble::tibble(ID = 1L, time = 24, evid = 0L, cmt = c(2L, 4L), DV = c(0.1, 100), mdv = 0L))
  expect_equal(obs_rows(time = c(24, 48), cmt = c(2, 4), DV = 0.1),
               tibble::tibble(ID = 1L, time = c(24, 24, 48, 48), evid = 0L, cmt = c(2L, 4L, 2L, 4L), DV = 0.1, mdv = 0L))

  # 3 multiples
  expect_equal(obs_rows(time = c(24, 48, 72), cmt = c(2,4), DV = c(0.123, 123, 0.456, 456, 0.789, 789)),
               tibble::tibble(ID = 1L, time = c(24, 24, 48, 48, 72, 72), evid = 0L, cmt = c(2L, 4L, 2L, 4L, 2L, 4L), DV = c(0.123, 123, 0.456, 456, 0.789, 789), mdv = 0L))

  expect_equal(obs_rows(time = c(24, 48), cmt = c(2, 4), DV = c(0.1, 100)),
               tibble::tibble(ID = 1L, time = c(24, 24, 48, 48), evid = 0L, cmt = c(2L, 4L, 2L, 4L), DV = c(0.1, 100, 0.1, 100), mdv = 0L))

  # Invalid number of arguments (length of vector cannot be recycled)
  expect_error(obs_rows(time = c(24, 48), cmt = c(2, 4), DV = c(0.1, 10, 100)), "arguments.*diff")
  expect_error(obs_rows(time = c(24, 48), cmt = 2, DV = c(0.1, 10, 100)), "arguments.*diff")
  expect_error(obs_rows(time = 24, cmt = c(1,2), DV = c(0.1, .2, .3)), "arguments.*diff")

  # Warning if DVmet created (use obs_rows.mrgmod instead)
  expect_warning(data_dvmet <- obs_rows(time = 0, cmt = 1, DVmet = 999),
                 "`DVmet` column added to the data. If you expected metabolite concentrations set in `DV`, `obs_rows\\(\\)` must be used with a 'mrgsolve' model.")
  #but as "covariate" still works though
  expect_equal(data_dvmet$DVmet, 999)

})

test_that("mdv and multiple cmt is ok", {
  expect_equal(obs_rows(time = 0, cmt = 1, DV = NA, mdv = 0)$mdv, 0L)
  expect_equal(obs_rows(time = 0, cmt = 1, DV = NA)$mdv, 1L)
  expect_equal(obs_rows(time = 0, cmt = 1)$mdv, 1L)
  expect_equal(obs_rows(time = 0, cmt = 1)$DV, NA_real_)

  expect_equal(obs_rows(time = 0, cmt = 1, DV = c(0.123, NA, 0.789))$mdv, c(0L, 1L, 0L))

  # Multicompartment ?
  # mdv will be repeated each
  actual_data <- obs_rows(time = c(24, 48, 72), cmt = c(2,4), DV = c(0.123, 123, 0.456, 456, 0.789, 789), mdv = c(0,1,0))
  expected_data <- tibble::tibble(ID = 1L, time = c(24, 24, 48, 48, 72, 72), evid = 0L,
                                  cmt = c(2L, 4L, 2L, 4L, 2L, 4L),
                                  DV = c(0.123, 123, 0.456, 456, 0.789, 789),
                                  mdv = c(0L, 0L, 1L, 1L, 0L, 0L))
  expect_equal(actual_data, expected_data)

  # no need to repeat mdv
  actual_data <- obs_rows(time = c(24, 48, 72), cmt = c(2,4), DV = c(0.123, 123, 0.456, 456, 0.789, 789), mdv = c(0,0,1,0,0,0))
  expected_data <- tibble::tibble(ID = 1L, time = c(24, 24, 48, 48, 72, 72), evid = 0L,
                                  cmt = c(2L, 4L, 2L, 4L, 2L, 4L),
                                  DV = c(0.123, 123, 0.456, 456, 0.789, 789),
                                  mdv = c(0L, 0L, 1L, 0L, 0L, 0L))
  expect_equal(actual_data, expected_data)

  #error if non-valid number of cmt
  expect_error(obs_rows(time = c(24, 48, 72), cmt = c(2,4), DV = c(0.123, 123, 0.456, 456, 0.789, 789), mdv = c(0,0,1,0,0,0,0)), "arguments.*diff")
})

test_that("obs_rows.mrgmod works", {
  model <- mcode("model", "
                 $CMT @annotated
                 DEPOT : Depot
                 CENTR : Central [OBS]
                 METAB : Central [OBS]
                 ", compile = FALSE)

  model_data <- obs_rows(model, time = 0)
  expect_s4_class(model_data, class = "mrgmod")
  expect_equal(get_data(model_data), tibble::tibble(ID = 1L, time = 0, evid = 0L, cmt = 2L, DV = NA_real_, mdv = 1L))

  # can we override default cmt
  expect_equal(get_data(obs_rows(model, time = 0, cmt = 999))$cmt, 999L)

  # DVmet works
  expect_equal(get_data(obs_rows(model, time = 0, DVmet = NA)),
               tibble::tibble(ID = 1L, time = 0, evid = 0L, cmt = c(2L, 3L), DV = c(NA_real_, NA_real_), mdv = 1L))
  expect_equal(get_data(obs_rows(model, time = c(0, 24, 48), DV = c(1,2,3), DVmet = c(10, NA, 30))),
               tibble::tibble(ID = 1L, time = c(0, 0, 24, 24, 48, 48), evid = 0L, cmt = c(2L, 3L, 2L, 3L, 2L, 3L),
                              DV = c(1, 10, 2, NA, 3, 30), mdv = c(0L, 0L, 0L, 1L, 0L, 0L)))
})

test_that(".datehour works in obs_rows()", {
  dh1 <- as.POSIXct("2022/01/01 10:00:00", tz = "UTC")
  dh2 <- as.POSIXct("2022/01/02 10:00:00", tz = "UTC")
  expect_equal(
    obs_rows(cmt = 2, .datehour = dh1, DV = 123) %>%
      obs_rows(cmt = 2, .datehour = dh2, DV = 456),
    tibble::tibble(ID = 1L, time = c(0,24), evid = 0L, cmt = 2L, DV = c(123,456), mdv = 0L, .datehour = c(dh1, dh2))
    )
})
