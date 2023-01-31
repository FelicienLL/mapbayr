test_that("lambda length is tested", {
  mod <- exmodel(add_exdata = F)
  dat <- exdata()

  expect_equal(
    preprocess.ofv.fix(x = mod, data = dat, lambda = .1)$lambda,
    .1
  )

  expect_error(
    preprocess.ofv.fix(x = mod, data = dat, lambda = c(.1, .2, .3, .4)),
    "\"lambda\" must be of length 1."
  )

})
