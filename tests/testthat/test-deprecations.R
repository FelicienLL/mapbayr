test_that("mbrest is deprecated", {
  expect_warning(my_mbrest <- mbrest(exmodel()), "Deprecated")
  expect_equal(my_mbrest$mapbay_tab, filter(est001$mapbay_tab, ID == 1))
})

test_that("adm_lines is deprecated", {
  expect_warning(ADM <- adm_lines(cmt = 1, amt = 100), "Deprecated")
  expect_equal(ADM, adm_rows(cmt = 1, amt = 100))
})

test_that("obs_lines is deprecated", {
  expect_warning(OBS <- obs_lines(time = 24, cmt = 1, DV = 100), "Deprecated")
  expect_equal(OBS, obs_rows(time = 24,cmt = 1, DV = 100))
})
