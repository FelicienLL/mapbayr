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
  mutate(ID = 6)

data12 <- bind_rows(data1, data2)

est12 <- mapbayest(mod, data12, verbose = F)

test_that("get_phi works", {

  phi12 <- get_phi(est12)
  expect_equal(nrow(phi12), 2)
  expect_equal(phi12$SUBJECT_NO, c(1,2))
  expect_equal(phi12$ID, c(1,6))
  expect_named(phi12, c("SUBJECT_NO", "ID", "ETA1", "ETA2", "ETA3", "ETC1_1", "ETC2_1", "ETC2_2", "ETC3_1", "ETC3_2", "ETC3_3", "OBJ"))

})
