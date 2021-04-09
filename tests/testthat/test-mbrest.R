test_that("mbest is deprecated", {
  mod <- mread("ex_mbr3", mbrlib())
  moddata <- mod %>%
    adm_lines(amt = 100) %>%
    obs_lines(time = 6, DV = .89)

  my_est <- mapbayest(moddata, verbose = F)

  expect_warning(my_mbrest <- mbrest(moddata, verbos = F), "deprecated")
  expect_equal(my_est$mapbay_tab, my_mbrest$mapbay_tab)
})
