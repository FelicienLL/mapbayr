test_that("new_ini2 works", {

  mod <- mread("ex_mbr3", mbrlib())

  data <- mod %>%
    adm_lines(amt = 100) %>%
    obs_lines(time = 12, DV = 3) %>%
    get_data()

  argofv <- c(preprocess.ofv.fix(x = mod), preprocess.ofv.id(x = mod, iddata = data))

  argopt <- preprocess.optim(x = mod, method = "L-BFGS-B", control = list(), force_initial_eta = NULL, hessian = F,
                             quantile_bound = .4) # !!

  ini <- new_ini2(arg.ofv = argofv, arg.optim = argopt, run = 1)

  expect_type(ini, "double")
  expect_true(all(ini > argopt$lower))
  expect_true(all(ini < argopt$upper))

})
