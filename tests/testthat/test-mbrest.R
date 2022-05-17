test_that("mbrest is deprecated", {
  expect_warning(my_mbrest <- mbrest(exmodel()), "Deprecated")
  expect_equal(my_mbrest$mapbay_tab, filter(est001$mapbay_tab, ID == 1))
})
