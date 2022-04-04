test_that("compute basic ofv", {
  mod <- mread('ex_mbr1', mbrlib())
  data1 <- mod %>%
    adm_lines(amt = 10, addl = 2, ii = 12) %>%
    obs_lines(DV = c(.1, .15), time = c(18, 40)) %>%
    add_covariates(list(WT = 70)) %>%
    get_data()
  arg.ofv <- c(preprocess.ofv.fix(x = mod), preprocess.ofv.id(x = mod, iddata = data1))

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
