test_that("compute basic ofv", {
  mod <- mread('ex_mbr1', mbrlib())
  data1 <- mod %>%
    adm_lines(amt = 10, addl = 2, ii = 12) %>%
    obs_lines(DV = c(.1, .15), time = c(18, 40)) %>%
    add_covariates(list(WT = 70)) %>%
    get_data()
  arg.ofv <- c(preprocess.ofv.fix(x = mod), preprocess.ofv.id(x = mod, iddata = data1))
  of_value <- compute_ofv(eta = c(ETA1 = -.2, ETA2 = .1, ETA3 = .2),
              mrgsolve_model = arg.ofv$mrgsolve_model,
              data = arg.ofv$data,
              sigma = arg.ofv$sigma,
              log_transformation = arg.ofv$log_transformation,
              DVobs = arg.ofv$DVobs,
              omega.inv = arg.ofv$omega.inv,
              obs_cmt = arg.ofv$obs_cmt)

  expect_equal(of_value, 0.433564742656389)
})
