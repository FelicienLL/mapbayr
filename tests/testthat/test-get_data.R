moddata1 <- exmodel(compile = FALSE)
test_that("get_data.mrgmod works", {
  expect_s3_class(get_data(moddata1), "tbl_df")
  expect_named(get_data(moddata1), c("ID", "time", "evid", "amt", "cmt", "ii", "addl", "mdv", "DV"))
})

data1 <- get_data.mrgmod(moddata1)

test_that("devalid_data_set works", {
  expect_equal(devalid_data_set(valid_data_set(data1, moddata1)), data1)
})

test_that("get_data.mapbayests works", {
  expect_s3_class(get_data(est001), "tbl_df")
  expect_named(get_data(est001), c("ID", "time", "evid", "amt", "cmt", "ii", "addl", "mdv", "DV"))

  #can return list (one element per ID)
  expect_length(data_list <- get_data(est001, output = "list"), 8)
  expect_equal(data_list[[1]], filter(get_data(est001, output = "df"), ID == 1))
})
