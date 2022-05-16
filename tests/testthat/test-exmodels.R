test_that("check_num works", {
  expect_error(check_num(1), NA)
  expect_error(check_num(2), "num must be selected from: 1, 6")
  expect_error(check_num(c(1, 6)), "num must be of length 1")
})

test_that("make_model_name works", {
  expect_match(make_model_name(1), "mrg_001.cpp")
  expect_match(make_model_name("1"), "mrg_001.cpp")
  expect_match(make_model_name("001"), "mrg_001.cpp")
})

test_that("make_data_name works", {
  expect_match(make_data_name(1), "data_to_fit001.csv")
  expect_match(make_data_name("1"), "data_to_fit001.csv")
  expect_match(make_data_name("001"), "data_to_fit001.csv")
})

test_that("clean_exdata works", {
  datatoclean <- tibble(rownum = 1:3, time = 72, evid = c(1,0,1), s2_sampling = c(1,2,3))
  datacleaned <- tibble(rownum = 2:3, time = 72, evid = c(0,1))
  expect_equal(clean_exdata(datatoclean), datacleaned)
})

test_that("exdata works", {
  expected_data <- data.frame(ID = 1,
                              time = c(0, 1.5, 4.4, 7.1, 24.6),
                              evid = c(1, rep(0,4)),
                              amt = c(10000, rep(0, 4)),
                              cmt = c(1, rep(2, 4)),
                              ii = 0, addl = 0,
                              mdv = c(1, rep(0, 4)),
                              DV = c(NA, 91.2904, 110.8260, 79.3840, 20.6671))

  expect_equal(exdata(), expected_data)
  expect_equal(exdata(num = 1), expected_data)
  expect_equal(unique(exdata(ID = 2)$ID), 2)
  expect_equal(unique(exdata(ID = 1:2)$ID), c(1,2))
  expect_equal(nrow(exdata(ID = 99)), 0)
  expect_equal(exdata(clean_data = FALSE)$s2_sampling, rep(1, 6))


})

test_that("exmodel works", {
  mod1 <- exmodel()
  expect_s4_class(mod1, "mrgmod")
  expect_s3_class(get_data(mod1), "data.frame")
  expect_equal(get_data(mod1)$ID, rep(1, 5)) #6 lines initially, 1 removed with cleaning

  mod2 <- exmodel(num = 6, ID = 2, clean_data = FALSE)
  expect_equal(names(mod2)$param, c("TVCL", "TVVC", "TVKA", "TVD2", "FR",  "ETA1", "ETA2", "ETA3", "ETA4"))
  expect_equal(get_data(mod2)$ID, rep(2, 5)) #5 lines

  expect_equal(dim(get_data(exmodel(add_exdata = FALSE))), c(0,0))

  expect_false(mrgsolve:::compiled.mrgmod(exmodel(compile = FALSE)))
})
