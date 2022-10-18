dh1 <- as.POSIXct("2022/01/01 10:00:00", tz = "UTC")
dh2 <- as.POSIXct("2022/01/02 10:00:00", tz = "UTC")
dh3 <- as.POSIXct("2022/01/03 10:00:00", tz = "UTC")

init_data <- tibble::tibble(ID = 1L, time = 0, evid = 1L, cmt = 1L, amt = 100, mdv = 1L)
init_data24 <- tibble::tibble(ID = 1L, time = 24, evid = 1L, cmt = 1L, amt = 100, mdv = 1L)
init_data48 <- tibble::tibble(ID = 1L, time = 48, evid = 1L, cmt = 1L, amt = 100, mdv = 1L)

init_data_dh <- tibble::tibble(ID = 1L, time = 0, evid = 1L, cmt = 1L, amt = 100, mdv = 1L, .datehour = dh1)
init_data_dh3 <- tibble::tibble(ID = 1L, time = 48, evid = 1L, cmt = 1L, amt = 100, mdv = 1L, .datehour = dh3)
init_data_dh3_t0 <- tibble::tibble(ID = 1L, time = 0, evid = 1L, cmt = 1L, amt = 100, mdv = 1L, .datehour = dh3)

test_that(".datehour works", {
  #1) time=NULL, no initial data
  expect_equal(
    datehour_manager(
      old_data = tibble::tibble(),
      time = NULL,
      .datehour = dh1),
    list(
      old_data = tibble::tibble(),
      time = 0,
      .datehour = dh1,
      dh0 = dh1
    )
  )

  #2) time=NULL, initial data without .datehour
  # ok if all time = 0
  expect_equal(
    datehour_manager(
      old_data = init_data,
      time = NULL,
      .datehour = dh1
    ),
    list(
      old_data = init_data,
      time = 0,
      .datehour = dh1,
      dh0 = dh1
    )
  )

  expect_equal(
    datehour_manager(
      old_data = init_data,
      time = NULL,
      .datehour = c(dh1, dh2)
    ),
    list(
      old_data = init_data,
      time = c(0, 24),
      .datehour = c(dh1, dh2),
      dh0 = dh1
    )
  )

  # error otherwise
  expect_error(
    datehour_manager(
      old_data = init_data24,
      time = NULL,
      .datehour = dh1),
    "Cannot assign when `.datehour` is in the timeline already defined by `time`."
  )

  #3) time=NULL, initial data with .datehour
  expect_equal(
    datehour_manager(
      old_data = init_data_dh,
      time = NULL,
      .datehour = dh2
    ),
    list(
      old_data = init_data_dh,
      time = 24,
      .datehour = dh2,
      dh0 = dh1
    )
  )

  expect_equal(
    datehour_manager(
      old_data = init_data_dh3_t0,
      time = NULL,
      .datehour = dh1
    ),
    list(
      old_data = mutate(init_data_dh3_t0, time = 48),
      time = 0,
      .datehour = dh1,
      dh0 = dh1
    )
  )

  #4) time non-NULL, no initial data
  expect_equal(
    datehour_manager(
      old_data = tibble::tibble(),
      time = 0,
      .datehour = dh1
    ),
    list(
      old_data = tibble::tibble(),
      time = 0,
      .datehour = dh1,
      dh0 = dh1
    )
  )

  expect_equal(
    datehour_manager(
      old_data = tibble::tibble(),
      time = 24,
      .datehour = dh2
    ),
    list(
      old_data = tibble::tibble(),
      time = 24,
      .datehour = dh2,
      dh0 = dh1
    )
  )

  expect_equal(
    datehour_manager(
      old_data = tibble::tibble(),
      time = c(0,24),
      .datehour = c(dh1, dh2)
    ),
    list(
      old_data = tibble::tibble(),
      time = c(0, 24),
      .datehour = c(dh1, dh2),
      dh0 = dh1
    )
  )

  expect_error(
    datehour_manager(
      old_data = tibble::tibble(),
      time = 0,
      .datehour = c(dh1, dh2)
    ),
    "`.time` and `.datehour` are of different length."
  )

  #5) time non-NULL, initial data without .datehour
  #>  if all times = 0
  expect_equal(
    datehour_manager(
      old_data = init_data,
      time = 0,
      .datehour = dh1
    ),
    list(
      old_data = init_data,
      time = 0,
      .datehour = dh1,
      dh0 = dh1
    )
  )

  #>  if initial time = 0, time > 0
  expect_equal(
    datehour_manager(
      old_data = init_data,
      time = 24,
      .datehour = dh2
    ),
    list(
      old_data = init_data,
      time = 24,
      .datehour = dh2,
      dh0 = dh1
    )
  )

  #>  if initial time > 0, time > 0
  expect_equal(
    datehour_manager(
      old_data = init_data48,
      time = 24,
      .datehour = dh2
    ),
    list(
      old_data = init_data48,
      time = 24,
      .datehour = dh2,
      dh0 = dh1
    )
  )

  #>  if initial time > 0, time = 0
  expect_equal(
    datehour_manager(
      old_data = init_data48,
      time = 0,
      .datehour = dh1
    ),
    list(
      old_data = init_data48,
      time = 0,
      .datehour = dh1,
      dh0 = dh1
    )
  )


  #6) time non-NULL, initial data with .datehour
  #> new time/datehour matching defined, but consistent so ok
  expect_equal(
    datehour_manager(
      old_data = init_data_dh,
      time = 24,
      .datehour = dh2
    ),
    list(
      old_data = init_data_dh,
      time = 24,
      .datehour = dh2,
      dh0 = NULL # do not need to define a new one, so NULL is returned
    )
  )

  expect_equal(
    datehour_manager(
      old_data = init_data_dh3,
      time = 24,
      .datehour = dh2
    ),
    list(
      old_data = init_data_dh3,
      time = 24,
      .datehour = dh2,
      dh0 = NULL
    )
  )

  #> new time/datehour matching and inconsistent
  expect_error(
    datehour_manager(
      old_data = init_data_dh,
      time = 9999,
      .datehour = dh2
    ),
    "`time` and `.datehour` are inconsistent with values already in the initial dataset."
  )
})


test_that("AOLA/TOLA works", {
  dat <- tibble::tibble(ID = c(1L, 1L, 1L, 1L, 1L, 3L, 3L, 3L, 3L),
                 time = c(0, 12, 24, 36, 48, 0, 24, 48, 60),
                 evid = c(1L, 0L, 1L, 0L, 1L, 1L, 1L, 1L, 0L),
                 cmt = 1L,
                 amt = c(100, 0, 200, 0, 300, 1000, 2000, 3000, 0))

  expect_equal(AOLA(dat)$AOLA, c(100, 100, 200, 200, 300, 1000, 2000, 3000, 3000))
  expect_equal(TOLA(dat)$TOLA, c(0, 0, 24, 24, 48, 0, 24, 48, 48))

  dataddl <- tibble::tibble(ID = 1L, amt = c(100, 0), time = c(0, 96), evid = c(1L, 0L), cmt = 1L, addl = c(3L, 0), ii = c(24, 0))

  expect_equal(AOLA(dataddl)$AOLA, c(100, 100))
  expect_equal(TOLA(dataddl)$TOLA, c(0, 24, 48, 72, 72))
})

test_that("NULL_remove works", {
  expect_equal(NULL_remove(list(A = NULL, B = NULL, C = "foo", D = "foo", "bar", NULL)), list(C = "foo", D = "foo", "bar"))
})

test_that("rearrange_nmdata works", {
  dat <- tibble::tibble(
    ID = c(3, 3, 3, 3, 3, 3, 1, 1),
    time = c(24, 0, 48, 0, 72, 0, 96, 0),
    evid = c(1, 1, 1, 0, 1, 0, 1, 0),
    cmt = c(2, 1, 3, 1, 2, 4, 2, 1)
  )

  # Sorts well
  expect_equal(
    rearrange_nmdata(dat),
    tibble::tibble(
      ID = c(1L, 1L, 3L, 3L, 3L, 3L, 3L, 3L),
      time = c(0, 96, 0, 0, 0, 24, 48, 72),
      evid = c(0L, 1L, 1L, 0L, 0L, 1L, 1L, 1L),
      cmt = c(1L, 2L, 1L, 1L, 4L, 2L, 3L, 2L)
    ))

  # .datehour increment works
  expect_equal(rearrange_nmdata(init_data, dh0 = NULL), init_data)
  expect_equal(rearrange_nmdata(init_data, dh0 = dh1), mutate(init_data, .datehour = dh1))
  dat2 <- bind_rows(init_data_dh, init_data24)
  expect_true(any(is.na(dat2$.datehour))) # Test if NA will be filled
  expect_equal(rearrange_nmdata(dat2), mutate(dat2, .datehour = c(dh1, dh2)))

  # nocb fill is correct
  dat3 <- tibble::tibble(ID = 1L, evid = 1L, time = c(0, 24, 48, 72, 96), cmt = 1L, amt = 1000,
                 BW = c(100, NA, NA, 200, NA))
  expect_equal(rearrange_nmdata(dat3)$BW, c(100, 200, 200, 200, 200))
})

test_that("cur_dh0 works", {
  expect_null(cur_dh0(data.frame(A = "foo")))
  expect_equal(cur_dh0(data.frame(time = c(0, 24), .datehour = c(dh1, dh2))), dh1)
  expect_equal(cur_dh0(data.frame(time = c(24, 48), .datehour = c(dh2, dh3))), dh1)
  expect_equal(cur_dh0(data.frame(time = c(24, 48, 72), .datehour = c(dh2, dh3, NA))), as.POSIXct(NA, "UTC"))
  expect_equal(cur_dh0(data.frame(time = c(24, 48, 72), .datehour = c(dh2, dh3, NA)), na.rm = TRUE), dh1)
})

test_that("forbidden covariate works", {
  expect_error(forbidden_covariate(list(A = "foo", B = "bar", C = "car", D = "dad"), cov = c("A", "B")),
               "Cannot have a covariate named: A B")
})

test_that("filter.mrgmod works", {
  mod <- mrgsolve::mcode("mod", "$CMT FOO", compile = FALSE)
  dat <- mod %>%
    adm_lines(amt = c(100, 200, 300), cmt = 1) %>%
    filter(amt != 200) %>%
    get_data()
  expect_equal(dat$amt, c(100, 300))
})
