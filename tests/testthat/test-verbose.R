mod <- exmodel(add_exdata = FALSE)
dat <- exdata(ID = 2) %>%
  mutate(DV = c(NA, 200))

test_that("verbose works on difficulty warning", {
  expect_message(mapbayest(mod, dat), "Reset with ")
  expect_message(mapbayest(mod, dat, verbose = FALSE), NA)
})
