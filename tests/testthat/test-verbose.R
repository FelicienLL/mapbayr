test_that("mapbayest verbose works", {
  mod <- mread('ex_mbr1', mbrlib())
  data <- mod %>%
    adm_lines(amt = 10, addl = 2, ii = 12) %>%
    obs_lines(DV = c(.1, .2), time = c(18, 40)) %>%
    get_data()

  expect_output(mapbayest(mod, data))
  expect_output(mapbayest(mod, data, verbose = T))
  expect_output(mapbayest(mod, data, verbose = F), regexp = NA)
})

test_that("verbose works on difficulty warning", {
  mod <- mread('ex_mbr1', mbrlib())
  data <- mod %>%
    adm_lines(amt = 10, addl = 2, ii = 12) %>%
    obs_lines(DV = c(5, 10), time = c(18, 40)) %>%
    get_data()

  invisible(capture.output(expect_message(mapbayest(mod, data), "Difficulty")))
  expect_warning(mapbayest(mod, data, verbose = FALSE), NA)
})
