mod <- mread('ex_mbr1', mbrlib())
data1 <- mod %>%
  adm_lines(amt = 10, addl = 2, ii = 12) %>%
  obs_lines(DV = c(.1, .15), time = c(18, 40)) %>%
  add_covariates(list(WT = 70)) %>%
  get_data()
arg.ofv <- c(preprocess.ofv.fix(x = mod), preprocess.ofv.id(x = mod, iddata = data1))

test_that("f works", {

  #on validated data
  expect_length(f(qmod = arg.ofv$qmod, data = arg.ofv$idvaliddata), 2)

  #on non-validated data
  expect_length(f(qmod = arg.ofv$qmod, data = data1), 2)
  expect_equal(
    f(qmod = arg.ofv$qmod, data = arg.ofv$idvaliddata),
    f(qmod = arg.ofv$qmod, data = data1)
  )

  #mdv != 0 are dealed ok
  validddatabis <- arg.ofv$idvaliddata
  validddatabis[5,"mdv"] <- 1
  expect_length(f(qmod = arg.ofv$qmod, data = validddatabis), 1)
  expect_equal(
    f(qmod = arg.ofv$qmod, data = arg.ofv$idvaliddata)[2],
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
  expectofv <- 0.433564742656389

  of_value1 <- compute_ofv(eta = c(ETA1 = -.2, ETA2 = .1, ETA3 = .2),
                          qmod = arg.ofv$qmod,
                          idvaliddata = arg.ofv$idvaliddata,
                          sigma = arg.ofv$sigma,
                          log_transformation = arg.ofv$log_transformation,
                          idDV = arg.ofv$idDV,
                          idcmt = arg.ofv$idcmt,
                          omega_inv = arg.ofv$omega_inv,
                          all_cmt = arg.ofv$all_cmt)

  expect_equal(of_value1, expectofv)

  of_value2 <- compute_ofv(eta = list(ETA1 = -.2, ETA2 = .1, ETA3 = .2),
                          qmod = arg.ofv$qmod,
                          idvaliddata = arg.ofv$idvaliddata,
                          sigma = arg.ofv$sigma,
                          log_transformation = arg.ofv$log_transformation,
                          idDV = arg.ofv$idDV,
                          idcmt = arg.ofv$idcmt,
                          omega_inv = arg.ofv$omega_inv,
                          all_cmt = arg.ofv$all_cmt)
  expect_equal(of_value2, expectofv)

  do_ofv1 <- do_compute_ofv(eta = c(ETA1 = -.2, ETA2 = .1, ETA3 = .2), argofv = arg.ofv)
  expect_equal(do_ofv1, expectofv)

  do_ofv2 <- do_compute_ofv(eta = list(ETA1 = -.2, ETA2 = .1, ETA3 = .2), argofv = arg.ofv)
  expect_equal(do_ofv2, expectofv)

})

