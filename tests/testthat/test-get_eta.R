mod <- mread('ex_mbr1', mbrlib())
data1 <- mod %>%
  adm_lines(amt = 10, addl = 2, ii = 12) %>%
  obs_lines(DV = c(.1, .2), time = c(18, 40)) %>%
  add_covariates(list(WT = 70)) %>%
  get_data()

data2 <- mod %>%
  adm_lines(amt = 10, addl = 2, ii = 12) %>%
  obs_lines(DV = c(.1, .2), time = c(19, 41)) %>%
  add_covariates(list(WT = 70)) %>%
  get_data() %>%
  mutate(ID = 6)

data12 <- bind_rows(data1, data2)

est1 <- mapbayest(mod, data1, verbose = F)
est12 <- mapbayest(mod, data12, verbose = F)

test_that("default get_eta works", {
  e1 <- get_eta(est1)
  e12 <- get_eta(est12)

  expect_length(e1, 3)
  expect_type(e1, "double")
  expect_named(e1, c("ETA1", "ETA2", "ETA3"))

  expect_length(e12, 1+3)
  expect_s3_class(e12, "data.frame")
  expect_named(e12, c("ID", "ETA1", "ETA2", "ETA3"))

})

test_that("get_eta list works", {
  e1 <- get_eta(est1, output = "list")
  e12 <- get_eta(est12, output = "list")

  expect_length(e1, 1)
  expect_type(e1, "list")
  expect_named(e1, c("1"))

  expect_length(e12, 2)
  expect_type(e12, "list")
  expect_named(e12, c("1", "6"))
})

test_that("get_eta num works", {
  e1 <- get_eta(est1, output = "num")

  expect_length(e1, 3)
  expect_type(e1, "double")
  expect_named(e1, c("ETA1", "ETA2", "ETA3"))

  expect_error(get_eta(est12, output = "num"), "Multiple ID, cannot coerce list to a vector of numeric.")
})

test_that("get_eta df works", {
  e1 <- get_eta(est1, output = "df")
  e12 <- get_eta(est12, output = "df")

  expect_length(e1, 4)
  expect_s3_class(e1, "data.frame")
  expect_named(e1, c("ID", "ETA1", "ETA2", "ETA3"))
  expect_equal(nrow(e1), 1)

  expect_length(e12, 4)
  expect_s3_class(e12, "data.frame")
  expect_named(e12, c("ID", "ETA1", "ETA2", "ETA3"))
  expect_equal(nrow(e12), 2)
})

test_that("get_eta stops if invalid type", {
  expect_error(get_eta(est1, output = "AAA"), "Allowed output are: ")

})

test_that("eta selection works", {
  e1 <- get_eta(est1, 1)
  e12 <- get_eta(est12, 1)

  expect_length(e1, 1)
  expect_type(e1, "double")
  expect_named(e1, c("ETA1"))

  expect_length(e12, 1+1)
  expect_s3_class(e12, "data.frame")
  expect_named(e12, c("ID", "ETA1"))


})

test_that("multiple eta selection works", {
  e1 <- get_eta(est1, 1, 3)
  e12 <- get_eta(est12, 1, 3)

  expect_length(e1, 2)
  expect_type(e1, "double")
  expect_named(e1, c("ETA1", "ETA3"))

  expect_length(e12, 1+2)
  expect_s3_class(e12, "data.frame")
  expect_named(e12, c("ID", "ETA1", "ETA3"))
})

test_that("eta selection works if selected twice", {
  e1 <- get_eta(est1, 1, 1)
  expect_length(e1, 1)
  expect_named(e1, c("ETA1"))
})

test_that("eta selection works if eta dont exist", {
  expect_equal(get_eta(est1), get_eta(est1, 4))
  expect_equal(get_eta(est1), get_eta(est1, "DONTEXIST"))
  expect_equal(get_eta(est1), get_eta(est1, "DONTEXIST", 4))
  expect_equal(get_eta(est1, 1), get_eta(est1, "DONTEXIST", 1))
})
