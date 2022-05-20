mod <- exmodel(401, ID = c(1,3))
dat <- get_data(mod)
est <- mapbayest(mod)

test_that("data argument works", {
  a1 <- augment(est)
  #even if no eta in data, does it predict individual ?
  a2 <- augment(est, data = dat)
  expect_equal(a1, a2)
  datb <- dat[1,] %>%
    mutate(time = 300) %>%
    bind_rows(dat, .)
  a3 <- augment(est, data = datb, delta = .1)
  expect_equal(max((a3$aug_tab[a3$aug_tab$ID==1,])$time), 300*1.2)
  expect_equal(a3$aug_tab[a3$aug_tab$time < 25 & a3$aug_tab$ID == 1,], a2$aug_tab[a2$aug_tab$time < 25 & a2$aug_tab$ID == 1,])

})

test_that("start argument works", {
  a1 <- augment(est)
  a2 <- augment(est, start = 0)
  expect_equal(a1, a2)

  a3 <- augment(est, start = 2)

  expect_equal(
    head(a3$aug_tab$time[a3$aug_tab$ID==1], 16),
    c(rep(1.5, 8), rep(2, 4), rep(2.1, 4))
  ) #observation times are preserved

  expect_equal(min(a3$aug_tab$time[a3$aug_tab$ID==3]), 2)

})

test_that("end argument works", {
  a1 <- augment(est)$aug_tab
  a2 <- augment(est, end = c(24.6, 222.6) * 1.2)$aug_tab
  expect_equal(a1, a2)

  a3 <- augment(est, end = 400)
  expect_equal(max(a3$aug_tab$time[a3$aug_tab$ID==1]), 400)
  expect_equal(max(a3$aug_tab$time[a3$aug_tab$ID==1]), 400)

  expect_error(augment(est, start = c(0, 100), end = c(100, 200)), NA)

})

test_that("delta argument works", {
  a1 <- augment(est)
  expect_equal(nrow(a1$aug_tab), 2320) #auto delta = .1

  a2 <- augment(est, delta = 10)
  expect_equal(nrow(a2$aug_tab), 184)

  a3 <- augment(est, end = 40000) # if end is high, no sim with small delta
  expect_lt(nrow(a3$aug_tab), 4000) #auto delta = 100
})

test_that("first prediction is not null if SS=1", {
  datss <- mutate(dat[dat$time==0,], ss = 1, ii = 24)
  augss <- augment(est, data = datss, start = 0, end = 3, delta = 1)$aug_tab
  expect_true(all(augss$value[augss$time == 0] != 0))
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

  expect_true(all(A2a$value_low[A2a$time!=0] != A2b$value_low[A2a$time!=0]))

  set.seed(2)
  A2c <- augment(est, delta = 1, ci = TRUE, ci_method = "simulations", ci_sims = 10)$aug_tab
  expect_equal(A2b, A2c)

  # Increased width of CI
  A95 <- augment(est, delta = 1, ci = TRUE, ci_width = 95)$aug_tab
  expect_true(all((A95[["value_up"]] >= A1a[["value_up"]])))
})

test_that("CI with multiple types of DV", {
  A1a <- augment(est, delta = 1, ci = TRUE)$aug_tab
  expect_length(unique(A1a$ID), 2)
  expect_true(all(A1a$value <= A1a$value_up))
  expect_true(all(A1a$value >= A1a$value_low))
})
