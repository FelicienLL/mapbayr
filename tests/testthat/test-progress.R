my_model <- exmodel(add_exdata = FALSE)
my_data <- exdata(ID = 1)

test_that("progress argument works", {
  my_data10 <- seq(10) %>%
    lapply(function(x) mutate(my_data, ID = x)) %>%
    bind_rows()

  expect_message(mapbayest(x = my_model, my_data10), "\\[=====")
  expect_message(mapbayest(x = my_model, my_data10, progress = FALSE), NA)
})

test_that("do_optimization works outside the call of mapbayest", {
  arg.ofv <- c(preprocess.ofv.fix(x = my_model, data = my_data), preprocess.ofv.id(x = my_model, iddata = my_data))
  arg.optim <- preprocess.optim(x = my_model)

  expect_error(do.call(do_optimization, c(arg.ofv, arg.optim)), NA)
})
