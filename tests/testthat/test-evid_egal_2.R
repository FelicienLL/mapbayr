test_that("evid = 2 are handled correctly", {
  mod3 <- mread("ex_mbr3", mbrlib())

  base_data <- mod3 %>%
    adm_lines(amt = 100) %>%
    obs_lines(time = 12, DV = 3) %>%
    obs_lines(time = 24, DV = 1) %>%
    see_data()

  data_evid2mdv0 <- base_data %>%
    mutate(evid = c(1,0,2))

  expect_error(mbrest(mod3, data = data_evid2mdv0, verbose = F))
  data_evid2mdv1 <- base_data %>%
    mutate(evid = c(1,0,2),
           mdv = c(1,0,1))
  expect_equal(length(mbrest(mod3, data_evid2mdv1, verbose = F)$arg.ofv.id[[1]]$DVobs), 1)
})
