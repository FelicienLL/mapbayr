test_that("get_data gets data", {
  mod1 <- mread("ex_mbr1", mbrlib())
  moddata1 <- mod1 %>%
    adm_lines(amt = 100) %>%
    obs_lines(time = c(1,5), DV = c(0.05, 0.3))

  expect_s3_class(get_data(moddata1), "tbl_df")


  %>%
    mapbayest()
})
