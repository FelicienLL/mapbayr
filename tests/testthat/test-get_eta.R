estid1 <- mapbayest(exmodel())
test_that("default get_eta works", {
  e1 <- get_eta(estid1)
  e8 <- get_eta(est001)

  expect_length(e1, 3)
  expect_type(e1, "double")
  expect_named(e1, c("ETA1", "ETA2", "ETA3"))

  expect_length(e8, 1+3)
  expect_equal(nrow(e8), 8)
  expect_s3_class(e8, "data.frame")
  expect_named(e8, c("ID", "ETA1", "ETA2", "ETA3"))

})

test_that("get_eta list works", {
  e1 <- get_eta(estid1, output = "list")
  e8 <- get_eta(est001, output = "list")

  expect_length(e1, 1)
  expect_type(e1, "list")
  expect_named(e1, "1")

  expect_length(e8, 8)
  expect_type(e8, "list")
  expect_named(e8, as.character(1:8))
})

test_that("get_eta num works", {
  e1 <- get_eta(estid1, output = "num")

  expect_length(e1, 3)
  expect_type(e1, "double")
  expect_named(e1, c("ETA1", "ETA2", "ETA3"))

  expect_error(get_eta(est001, output = "num"), "Multiple ID, cannot coerce list to a vector of numeric.")
})

test_that("get_eta df works", {
  e1 <- get_eta(estid1, output = "df")
  e8 <- get_eta(est001, output = "df")

  expect_length(e1, 4)
  expect_s3_class(e1, "data.frame")
  expect_named(e1, c("ID", "ETA1", "ETA2", "ETA3"))
  expect_equal(nrow(e1), 1)

  expect_length(e8, 4)
  expect_s3_class(e8, "data.frame")
  expect_named(e8, c("ID", "ETA1", "ETA2", "ETA3"))
  expect_equal(nrow(e8), 8)
})

test_that("get_eta stops if invalid type", {
  expect_error(get_eta(estid1, output = "AAA"), "Allowed output are: ")

})

test_that("eta selection works", {
  e1 <- get_eta(estid1, 1)
  e8 <- get_eta(est001, 1)

  expect_length(e1, 1)
  expect_type(e1, "double")
  expect_named(e1, c("ETA1"))

  expect_length(e8, 1+1)
  expect_s3_class(e8, "data.frame")
  expect_named(e8, c("ID", "ETA1"))


})

test_that("multiple eta selection works", {
  e1 <- get_eta(estid1, 1, 3)
  e8 <- get_eta(est001, 1, 3)

  expect_length(e1, 2)
  expect_type(e1, "double")
  expect_named(e1, c("ETA1", "ETA3"))

  expect_length(e8, 1+2)
  expect_s3_class(e8, "data.frame")
  expect_named(e8, c("ID", "ETA1", "ETA3"))
})

test_that("eta selection works if selected twice", {
  e1 <- get_eta(estid1, 1, 1)
  expect_length(e1, 1)
  expect_named(e1, c("ETA1"))
})

test_that("eta selection works if eta dont exist", {
  expect_equal(get_eta(estid1), get_eta(estid1, 4))
  expect_equal(get_eta(estid1), get_eta(estid1, "DONTEXIST"))
  expect_equal(get_eta(estid1), get_eta(estid1, "DONTEXIST", 4))
  expect_equal(get_eta(estid1, 1), get_eta(estid1, "DONTEXIST", 1))
})
