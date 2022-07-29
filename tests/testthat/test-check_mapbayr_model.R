test_that("missing shared object is checked", {
  model_first <- exmodel(cache = FALSE)
  model_second <- exmodel(cache = FALSE)

  expect_error(check_mapbayr_model(model_first, check_compile = TRUE), ".*\\[loadso\\] the model dll file doesn\\'t exist")
  expect_error(check_mapbayr_model(model_first, check_compile = FALSE), NA)

})
