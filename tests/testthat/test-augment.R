mod <- mread('ex_mbr2', mbrlib())
data1 <- mod %>%
  adm_lines(amt = 10, addl = 2, ii = 12) %>%
  obs_lines(DV = c(.1, .2), time = c(18, 41)) %>%
  add_covariates(list(WT = 70)) %>%
  get_data()

data2 <- mod %>%
  adm_lines(amt = 10, addl = 2, ii = 12) %>%
  obs_lines(DV = c(.45, .35), time = c(19, 41), DVmet = c(.15, .25)) %>%
  add_covariates(list(WT = 70)) %>%
  get_data() %>%
  mutate(ID = 2)

data12 <- bind_rows(data1, data2)
est <- mapbayest(x = mod, data = data12, verbose = F)

test_that("data argument works", {
  a1 <- augment(est)
  #even if no eta in data, does it predict individual ?
  a2 <- augment(est, data = data12)
  expect_equal(a1, a2)
  data12b <- data12[1,] %>%
    mutate(time = 72) %>%
    bind_rows(data12, .)
  a3 <- augment(est, data = data12b, delta = .1)
  expect_equal(filter(a3$aug_tab, time<41), filter(a2$aug_tab, time<41))
  #after t=41, mdv = 1 in a3 because of the last line (an administration) that is locf-ed.
})

test_that("end argument works", {
  a1 <- augment(est)
  a2 <- augment(est, end = 41*1.2)
  expect_equal(a1, a2)

  a3 <- augment(est, end = 400)
  expect_equal(max(filter(a3$aug_tab, ID == 1)$time), 400)
  expect_equal(max(filter(a3$aug_tab, ID == 2)$time), 400)

})

test_that("delta argument works", {
  a1 <- augment(est)
  expect_equal(nrow(a1$aug_tab), 8*2*2 + 2*41*1.2*10*2*2) #auto delta = .1

  a2 <- augment(est, delta = 10)
  expect_equal(nrow(a2$aug_tab), 8*2*2 + 2*4*2*2)

  a3 <- augment(est, end = 40000) # if end is high, no sim with small delta
  expect_lt(nrow(a3$aug_tab), 4000) #auto delta = 100
})



