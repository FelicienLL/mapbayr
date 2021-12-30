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
  mutate(ID = 3)

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

test_that("start argument works", {
  a1 <- augment(est)
  a2 <- augment(est, start = 0)
  expect_equal(a1, a2)

  a3 <- augment(est, start = 10)
  expect_equal(min(filter(a3$aug_tab, ID == 1)$time), 10)
  expect_equal(min(filter(a3$aug_tab, ID == 2)$time), 10)

})

test_that("end argument works", {
  a1 <- augment(est)
  a2 <- augment(est, end = 41*1.2)
  expect_equal(a1, a2)

  a3 <- augment(est, end = 400)
  expect_equal(max(filter(a3$aug_tab, ID == 1)$time), 400)
  expect_equal(max(filter(a3$aug_tab, ID == 2)$time), 400)

  expect_error(augment(est, start = c(0, 100), end = c(100, 200)), NA)

})

test_that("delta argument works", {
  a1 <- augment(est)
  expect_equal(nrow(a1$aug_tab), 8*2*2 + 2*41*1.2*10*2*2) #auto delta = .1

  a2 <- augment(est, delta = 10)
  expect_equal(nrow(a2$aug_tab), 8*2*2 + 2*4*2*2)

  a3 <- augment(est, end = 40000) # if end is high, no sim with small delta
  expect_lt(nrow(a3$aug_tab), 4000) #auto delta = 100
})

test_that("first prediction is not null if SS=0", {
  est1 <- mod %>%
    adm_lines(amt = 10, ss = 1, ii = 24) %>%
    obs_lines(time = 12, DV = 1.1) %>%
    mapbayest(verbose = F)
  expect_true(all(filter(augment(est1)$aug_tab, time == 0)$value != 0))
})

test_that("confidence interval works", {
  A0 <- augment(est, delta = 1)$aug_tab #default : no conf interval
  expect_null(A0[["value_up"]])
  expect_null(A0[["value_low"]])

  #Default CI method
  set.seed(1)
  A1a <- augment(est, delta = 1, ci = TRUE)$aug_tab
  set.seed(2)
  A1b <- augment(est, delta = 1, ci = TRUE)$aug_tab

  expect_type(A1a[["value_low"]], "double")
  expect_equal(A1a, A1b)

  #Simulations = different results unless seed is fixed
  set.seed(1)
  A2a <- augment(est, delta = 1, ci = TRUE, ci_method = "simulations", ci_sims = 10)$aug_tab
  set.seed(2)
  A2b <- augment(est, delta = 1, ci = TRUE, ci_method = "simulations", ci_sims = 10)$aug_tab

  expect_true(all(filter(A2a, time != 0)$value_low != filter(A2b, time != 0)$value_low))

  set.seed(2)
  A2c <- augment(est, delta = 1, ci = TRUE, ci_method = "simulations", ci_sims = 10)$aug_tab
  expect_equal(A2b, A2c)

  # Increased width of CI
  A95 <- augment(est, delta = 1, ci = TRUE, ci_width = 95)$aug_tab
  expect_true(all((A95[["value_up"]] >= A1a[["value_up"]])))
})

# test_that("CI with multiple types of DV", {
#   A1a <- augment(est, delta = 1, ci = TRUE)$aug_tab
#   expect_length(unique(A1a$ID), 2)
#   expect_true(all(A1a$DV <= A1a$value_up))
#   expect_true(all(A1a$DV >= A1a$value_low))
# })
