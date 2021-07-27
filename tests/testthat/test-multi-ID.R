test_that("mapbayr fits multiple ID", {
  mod <- mread('ex_mbr1', mbrlib())
  data1 <- mod %>%
    adm_lines(amt = 10, addl = 2, ii = 12) %>%
    obs_lines(DV = c(.1, .2), time = c(18, 40)) %>%
    add_covariates(list(WT = 70)) %>%
    get_data()

  data2 <- mod %>%
    adm_lines(amt = 10, addl = 2, ii = 12) %>%
    obs_lines(DV = c(.1, .2), time = c(19, 41)) %>%
    add_covariates(list(WT = 70)) %>%
    get_data() %>%
    mutate(ID = 2)

  data12 <- bind_rows(data1, data2)

  expect_error(est <- mapbayest(x = mod, data = data12, verbose = F), NA)
  expect_equal(length(est$final_eta), 2)
  expect_error(esttab <- mapbayest(x = mod, data = data12, output = "df", verbose = F), NA)
  expect_equal(data12$ID,   esttab$ID)
  expect_equal(data12$time, esttab$time  )
  expect_equal(data12$evid, esttab$evid)
  expect_equal(data12$amt,  esttab$amt)
  expect_equal(data12$cmt,  esttab$cmt)
  expect_equal(data12$mdv,  esttab$mdv)
  expect_equal(data12$DV,   esttab$DV)
})

test_that("order of IDs is preserved", {
  mod <- mread('ex_mbr1', mbrlib())
  data1 <- mod %>%
    adm_lines(amt = 10, addl = 2, ii = 12) %>%
    obs_lines(DV = c(0.08, .1), time = c(18, 40)) %>%
    add_covariates(list(WT = 70)) %>%
    get_data()

  data30 <- mod %>%
    adm_lines(amt = 100, addl = 2, ii = 12) %>%
    obs_lines(DV = c(.8, 1), time = c(19, 41)) %>%
    add_covariates(list(WT = 90)) %>%
    get_data() %>%
    mutate(ID = 30)

  data2 <- mod %>%
    adm_lines(amt = 100, addl = 2, ii = 12) %>%
    obs_lines(DV = c(.9, 1.1), time = c(15, 36)) %>%
    add_covariates(list(WT = 50)) %>%
    get_data() %>%
    mutate(ID = 2)

  data_all <- bind_rows(data1, data30, data2)

  est1 <-  mapbayest(mod, data1, verbose = F)$final_eta[[1]]
  est30 <-  mapbayest(mod, data30, verbose = F)$final_eta[[1]]
  est2 <-  mapbayest(mod, data2, verbose = F)$final_eta[[1]]

  est_all <- mapbayest(mod, data_all, verbose = F)$final_eta

  est_all_1 <- est_all[[1]]
  est_all_30 <- est_all[[2]]
  est_all_2 <- est_all[[3]]

  expect_equal(est1, est_all_1)
  expect_equal(est30, est_all_30)
  expect_equal(est2, est_all_2)

})
