mod <- exmodel(1, add_exdata = FALSE, compile = FALSE)
test_that("non-allowed method are detected", {
  expect_error(preprocess.optim(mod, "L-BFGS-B"), NA)
  expect_error(preprocess.optim(mod, "newuoa"), NA)
  expect_error(preprocess.optim(mod, "blabla"), "Accepted method")
})

test_that("it creates defaults args for L-BFGS-B", {
  args <- preprocess.optim(mod, "L-BFGS-B")
  expect_named(args, c("par", "fn", "method", "control", "lower", "upper"))
  expect_equal(args$par, c(ETA1 = 0, ETA2 = 0, ETA3 = 0))
  expect_equal(args$fn, mapbayr::compute_ofv)
  expect_equal(args$method, "L-BFGS-B")
  expect_equal(args$control, list(trace = 0, maxit = 9999, fnscale = 0.001, lmm = 7))
  expect_equal(args$lower, -args$upper)
})

test_that("it modifies args for L-BFGS-B", {
  args_ctrl <- preprocess.optim(mod, "L-BFGS-B", control = list(fnscale = 0.1, factr = 1e5))
  expect_equal(args_ctrl$control, list(fnscale = 0.1, factr = 1e5, trace = 0, maxit = 9999, lmm = 7))

  forced_eta <- rename_as_eta(c(0.123, 0.456, 0.789))
  args_initial_eta <- preprocess.optim(mod, "L-BFGS-B", force_initial_eta = forced_eta)
  expect_equal(args_initial_eta$par, forced_eta)

  args_bound <- preprocess.optim(mod, "L-BFGS-B", quantile_bound = 0.000001)
  expect_gt(args_bound$upper[1], args_ctrl$upper[1])
  expect_lt(args_bound$lower[1], args_ctrl$lower[1])
})

test_that("it creates defaults args for newuoa", {
  args <- preprocess.optim(mod, "newuoa")
  expect_named(args, c("par", "fn", "control", "method"))
  expect_equal(args$par, c(ETA1 = 0.01, ETA2 = 0.01, ETA3 = 0.01))
  expect_equal(args$fn, mapbayr::compute_ofv)
  expect_equal(args$method, "newuoa")
  expect_equal(args$control, list(iprint = 0))
})

test_that("it modifies args for newuoa", {
  args_ctrl <- preprocess.optim(mod, "newuoa", control = list(maxfun = 9999, iprint = 1))
  expect_equal(args_ctrl$control, list(maxfun = 9999, iprint = 1))
})


