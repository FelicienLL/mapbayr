test_that("plot works", {
  mod3 <- mread("ex_mbr3", mbrlib())
  est <- mod3 %>%
    adm_lines(amt = 100) %>%
    obs_lines(time = 24, DV = 1.1) %>%
    mapbayest(verbose = F)

  expect_message(p3 <- plot(est), NA)
  expect_s3_class(p3, "ggplot")

})
