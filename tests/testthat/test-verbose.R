mod <- mread('ex_mbr1', mbrlib())
data <- mod %>%
    adm_lines(amt = 10, addl = 2, ii = 12) %>%
    obs_lines(DV = c(5, 10), time = c(18, 40)) %>%
    get_data()

test_that("verbose works on difficulty warning", {
  expect_message(mapbayest(mod, data), "Reset with ")
  expect_message(mapbayest(mod, data, verbose = FALSE), NA)
})
