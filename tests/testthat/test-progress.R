my_model <- exmodel(add_exdata = FALSE)
my_data <- exdata(ID = 1)

test_that("progress argument works", {
  testthat::skip("cannot test progress bar")
  # works manually, but not through automatic testing procedures

  my_data10 <- seq(10) %>%
    map_dfr(.f = ~ mutate(my_data, ID = .x))

  expect_message(mapbayest(x = my_model, my_data10), "\\[=====")
  expect_message(mapbayest(x = my_model, my_data10, progress = FALSE), NA)
})

test_that("do_optimization works outside the call of mapbayest", {
  arg.ofv <- c(preprocess.ofv.fix(x = my_model, data = my_data), preprocess.ofv.id(x = my_model, iddata = my_data))
  arg.optim <- preprocess.optim(x = my_model, method = "L-BFGS-B", control = list(), force_initial_eta = NULL, quantile_bound = 0.001)

  expect_error(do.call(do_optimization, c(arg.ofv, arg.optim)), NA)
})
