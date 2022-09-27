test_that("obs_lines works", {
  mod <- exmodel(add_exdata = FALSE)
  data <- get_data(obs_lines(mod, time = 24, DV = c(NA, 123)))
  expect_equal(data$mdv, c(1, 0))
})
