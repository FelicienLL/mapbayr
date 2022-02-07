mod1 <- mread("ex_mbr1", mbrlib())
moddata1 <- mod1 %>%
  adm_lines(amt = 100) %>%
  obs_lines(time = c(1,5), DV = c(0.05, 0.3))
est1 <- mapbayest(moddata1, verbose = F)

test_that("get_data.mrgmod works", {
  expect_s3_class(get_data(moddata1), "tbl_df")
  expect_named(get_data(moddata1), c("ID", "time", "evid", "mdv", "amt", "cmt", "rate", "DV"))
})

test_that("get_data.mapbayests works", {
  expect_s3_class(get_data(est1), "tbl_df")
  expect_named(get_data(est1), c("ID", "time", "evid", "mdv", "amt", "cmt", "rate", "DV"))

  #can return list (one element per ID)
  expect_length(data_list <- get_data(est1, output = "list"), 1)
  expect_equal(data_list[[1]], get_data(est1, output = "df"))
})

test_that("see_data is deprecated", {
  expect_warning(see_mod1 <- see_data(moddata1), "see_data\\(\\) is deprecated. Use get_data\\(\\) instead.")
  expect_equal(see_mod1, get_data(moddata1))

  expect_warning(see_est1 <- see_data(est1), "see_data\\(\\) is deprecated. Use get_data\\(\\) instead.")
  expect_equal(see_est1, get_data(est1))
})

