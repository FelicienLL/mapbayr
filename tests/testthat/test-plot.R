est <- mapbayest(exmodel())
test_that("plot works", {
  expect_message(p3 <- plot(est), NA)
  expect_s3_class(p3, "ggplot")
})

test_that("no warning if DV is NA and mdv = 1", {
  # Fix 114
  est$mapbay_tab[5, "mdv"] <- 1
  est$mapbay_tab[5, "DV"] <- NA
  expect_warning(plot(est), NA)
})
