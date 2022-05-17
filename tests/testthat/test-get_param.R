mod <- exmodel(capture = c("CL", "TVCL"), add_exdata = FALSE, cache = FALSE)
dat <- exdata(ID = 1:2)
est1 <- mapbayest(mod, dat[dat$ID==1,])
est12 <- mapbayest(mod, dat)

test_that("get param output", {
  #Vector of numeric
  expect_type(get_param(est1, output = "num"), "double")
  expect_error(get_param(est12, output = "num"), "Multiple ID, cannot coerce list to a vector of numeric.")

  #df
  expect_s3_class(get_param(est1, output = "df"), "data.frame")
  expect_s3_class(get_param(est12, output = "df"), "data.frame")

})


test_that("get_param obeys to keep_ argument", {

  expect_equal(get_param(est1, "CL"), 5.9975519)
  expect_equal(get_param(est1, "CL", keep_ID = TRUE), c(1, 5.9975519))
  expect_equal(get_param(est1, "CL", keep_ID = FALSE), 5.9975519)
  expect_equal(get_param(est1, "CL", keep_names = TRUE), c(CL = 5.9975519))
  expect_equal(get_param(est1, "CL", keep_names = FALSE), 5.9975519)
  expect_equal(get_param(est1, "CL", keep_names = TRUE, keep_ID = TRUE), c(ID = 1, CL = 5.9975519))
  expect_equal(get_param(est1, "CL", keep_names = TRUE, keep_ID = FALSE), c(CL = 5.9975519))
  expect_equal(get_param(est1, "CL", keep_names = FALSE, keep_ID = TRUE), c(1, 5.9975519))
  expect_equal(get_param(est1, "CL", keep_names = FALSE, keep_ID = FALSE), 5.9975519)

  expect_equal(get_param(est1, "CL", "TVCL"), c(CL = 5.9975519, TVCL = 4))
  expect_equal(get_param(est1, "CL", "TVCL", keep_ID = TRUE), c(ID = 1, CL = 5.9975519, TVCL = 4))
  expect_equal(get_param(est1, "CL", "TVCL", keep_names = TRUE), c(CL = 5.9975519, TVCL = 4))
  expect_equal(get_param(est1, "CL", "TVCL", keep_names = FALSE), c(5.9975519, 4))
  expect_equal(get_param(est1, "CL", "TVCL", keep_names = TRUE, keep_ID = TRUE), c(ID = 1, CL = 5.9975519, TVCL = 4))
  expect_equal(get_param(est1, "CL", "TVCL", keep_names = TRUE, keep_ID = FALSE), c(CL = 5.9975519, TVCL = 4))
  expect_equal(get_param(est1, "CL", "TVCL", keep_names = FALSE, keep_ID = TRUE), c(1, 5.9975519, 4))
  expect_equal(get_param(est1, "CL", "TVCL", keep_names = FALSE, keep_ID = FALSE), c(5.9975519, 4))

  expect_equal(get_param(est12, "CL"), data.frame(ID = c(1,2), CL = c(5.9975519, 3.4588236)))
  expect_equal(get_param(est12, "CL", keep_ID = TRUE), data.frame(ID = c(1,2), CL = c(5.9975519, 3.4588236)))
  expect_equal(get_param(est12, "CL", keep_ID = FALSE), data.frame(CL = c(5.9975519, 3.4588236)))
  expect_equal(get_param(est12, "CL", keep_names = TRUE), data.frame(ID = c(1,2), CL = c(5.9975519, 3.4588236)))
  expect_equal(get_param(est12, "CL", keep_names = FALSE), unname(data.frame(c(1,2),c(5.9975519, 3.4588236))))
  expect_equal(get_param(est12, "CL", keep_names = TRUE, keep_ID = TRUE), data.frame(ID = c(1,2), CL = c(5.9975519, 3.4588236)))
  expect_equal(get_param(est12, "CL", keep_names = TRUE, keep_ID = FALSE), data.frame(CL = c(5.9975519, 3.4588236)))
  expect_equal(get_param(est12, "CL", keep_names = FALSE, keep_ID = TRUE), unname(data.frame(c(1,2),c(5.9975519, 3.4588236))))
  expect_equal(get_param(est12, "CL", keep_names = FALSE, keep_ID = FALSE), unname(data.frame(c(5.9975519, 3.4588236))))

})

