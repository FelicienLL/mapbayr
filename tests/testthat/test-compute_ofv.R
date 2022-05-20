est <- mapbayest(exmodel())
dat <- get_data(est)
arg.ofv <- c(est$arg.ofv.fix, est$arg.ofv.id[[1]])

test_that("f works", {

  #on validated data
  expect_length(f(qmod = arg.ofv$qmod, data = arg.ofv$idvaliddata), 4)

  #on non-validated data
  expect_length(f(qmod = arg.ofv$qmod, data = dat), 4)
  expect_equal(
    f(qmod = arg.ofv$qmod, data = arg.ofv$idvaliddata),
    f(qmod = arg.ofv$qmod, data = dat)
  )

  #mdv != 0 are dealed ok
  validddatabis <- arg.ofv$idvaliddata
  validddatabis[5,"mdv"] <- 1
  expect_length(f(qmod = arg.ofv$qmod, data = validddatabis), 4-1)
  expect_equal(
    f(qmod = arg.ofv$qmod, data = arg.ofv$idvaliddata)[1:3],
    f(qmod = arg.ofv$qmod, data = validddatabis)
  )
})

test_that("h matrix computation works", {
  expectmat <- matrix(c(400, 0, 200, 0, 1, 0, 1, 0, 0, 40, 0, 20, 0, 1, 0, 1), ncol = 4, nrow = 4)

  expect_equal(h(pred = c(400, 40, 200, 20), cmt = c(2, 3, 2, 3), all_cmt = c(2, 3)), expectmat)

  expectmat[1,1] <- 1
  expect_equal(h(pred = c(0, 40, 200, 20), cmt = c(2, 3, 2, 3), all_cmt = c(2, 3)), expectmat)
})



test_that("compute basic ofv", {
  expecteta <- c(
    ETA1 = 0.4050570108131819058,
    ETA2 = 0.07285029212110255559,
    ETA3 = -0.07495339444174362042
    )

  expectofv <- 22.29351276398179493

  of_value1 <- compute_ofv(
    eta = expecteta,

    qmod = arg.ofv$qmod, idvaliddata = arg.ofv$idvaliddata,
    sigma = arg.ofv$sigma, log_transformation = arg.ofv$log_transformation,
    idDV = arg.ofv$idDV, idcmt = arg.ofv$idcmt,
    omega_inv = arg.ofv$omega_inv, all_cmt = arg.ofv$all_cmt)

  expect_equal(of_value1, expectofv)

  of_value2 <- compute_ofv(
    eta = as.list(expecteta),

    qmod = arg.ofv$qmod, idvaliddata = arg.ofv$idvaliddata,
    sigma = arg.ofv$sigma, log_transformation = arg.ofv$log_transformation,
    idDV = arg.ofv$idDV, idcmt = arg.ofv$idcmt,
    omega_inv = arg.ofv$omega_inv, all_cmt = arg.ofv$all_cmt)

  expect_equal(of_value2, expectofv)

  do_ofv1 <- do_compute_ofv(eta = expecteta, argofv = arg.ofv)
  expect_equal(do_ofv1, expectofv)

  do_ofv2 <- do_compute_ofv(eta = expecteta, argofv = arg.ofv)
  expect_equal(do_ofv2, expectofv)

})

