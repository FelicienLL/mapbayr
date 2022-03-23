test_that("new_ini2 works", {

  mod <- mread("ex_mbr3", mbrlib())

  data <- mod %>%
    adm_lines(amt = 100) %>%
    obs_lines(time = 12, DV = 3) %>%
    obs_lines(time = 24, DV = 1) %>%
    get_data()

  argofv <- c(preprocess.ofv.fix(x = mod), preprocess.ofv.id(x = mod, iddata = data))

  argopt <- preprocess.optim(x = mod, method = "L-BFGS-B", control = list(), force_initial_eta = NULL,
                             quantile_bound = .4) # !!


  ini2 <- new_ini2(arg.ofv = argofv, arg.optim = argopt, run = 1)

  ini3 <- new_ini3(arg.ofv = argofv, arg.optim = argopt, run = 1)

  expect_equal(ini2, ini3)
  expect_type(ini3, "double")
  expect_true(all(ini3 > argopt$lower))
  expect_true(all(ini3 < argopt$upper))

})

