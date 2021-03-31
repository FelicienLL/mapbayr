test_that("MDV == 0 are handled properly", {
  mod3 <- mread("ex_mbr3", mbrlib())

  base_data <- mod3 %>%
    adm_lines(amt = 100) %>%
    obs_lines(time = 11, DV = 3) %>%
    obs_lines(time = 12, DV = 3) %>%
    obs_lines(time = 24, DV = 1) %>%
    see_data() #Three obs taken into account (1)

  data_MDV1 <- base_data %>%
    mutate(mdv = c(1,1,0,0))  #Two obs taken into account, one ignored (2)

  data_MDV1_CL <- data_MDV1 %>%
    mutate(TVCL = c(.7, 2, .7, .7)) #Still one ignored , but another info on CL (3) :the line must be read by mrgsim, but not for OFV

  data_noline_CL <- data_MDV1_CL %>%
    filter(time != 11) #The line is just dropped (4)

  est1 <- mbrest(mod3, data = base_data, verbose = F)
  est2 <- mbrest(mod3, data = data_MDV1, verbose = F)
  est3 <- mbrest(mod3, data = data_MDV1_CL, verbose = F)
  est4 <- mbrest(mod3, data = data_noline_CL, verbose = F)

  #Test number of observation fitted
  expect_equal(length(est1$arg.ofv.id[[1]]$DVobs), 3)
  expect_equal(length(est2$arg.ofv.id[[1]]$DVobs), 2)
  expect_equal(length(est3$arg.ofv.id[[1]]$DVobs), 2)
  expect_equal(length(est4$arg.ofv.id[[1]]$DVobs), 2)

  #Test different eta are found
  # 1 != 2
  # 1 != 3
  # 1 != 4
  # 2 != 3
  # 2 == 4
  # 3 != 4

  expect_false(isTRUE(all.equal(est1$final_eta[[1]], est2$final_eta[[1]])))
  expect_false(isTRUE(all.equal(est1$final_eta[[1]], est3$final_eta[[1]])))
  expect_false(isTRUE(all.equal(est1$final_eta[[1]], est4$final_eta[[1]])))
  expect_false(isTRUE(all.equal(est2$final_eta[[1]], est3$final_eta[[1]])))
  expect_true(isTRUE(all.equal(est2$final_eta[[1]], est4$final_eta[[1]])))
  expect_false(isTRUE(all.equal(est3$final_eta[[1]], est4$final_eta[[1]])))
  })
