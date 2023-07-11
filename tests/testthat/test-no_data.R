mod <- exmodel(add_exdata = FALSE)

test_that("stop if no data", {
  expect_error(mapbayest(mod), "No data provided")
})

test_that("don't stop if no observation", {

  dat <- exdata(ID = 2)

  # No observation line
  dat0 <- filter(dat, evid == 1)
  expect_error(est3 <- mapbayest(mod, dat0, verbose = FALSE), NA)
  expect_equal(get_eta(est3), c(ETA1 = 0, ETA2 = 0, ETA3 = 0), tolerance = 10^-6)
  expect_error(plot(est3), NA)
  expect_warning(p3 <- plot(est3), NA)
  expect_s3_class(p3, "ggplot")

  # Observation line with MDV = 1
  dat1 <- mutate(dat, mdv = 1)
  expect_error(est4 <- mapbayest(mod, dat1, verbose = F), NA)
  expect_equal(get_eta(est4), c(ETA1 = 0, ETA2 = 0, ETA3 = 0), tolerance = 10^-6)
  expect_error(plot(est4), NA)
  expect_warning(p4 <- plot(est4), NA)
  expect_s3_class(p4, "ggplot")

})
