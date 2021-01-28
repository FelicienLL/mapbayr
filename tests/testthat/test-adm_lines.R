mod1 <- mread('ex_mbr1', mbrlib())
mod2 <- mread('ex_mbr2', mbrlib())
mod3 <- mread('ex_mbr3', mbrlib())

test_that("example models are suitable for these tests", {
  #Administration compartment
  expect_equal(adm_cmt(mod1), c(1,2))
  expect_equal(adm_cmt(mod2), 1)
  expect_equal(adm_cmt(mod3), 1)

  #Zero order infusion to estimate => rate = -2
  expect_equal(adm_0_cmt(mod1), 2)
  expect_equal(adm_0_cmt(mod2), 1)
  expect_null(adm_0_cmt(mod3))
})



test_that("detection of default administration compartment is good",{
  expect_equal(see_data(adm_lines(mod1, amt = 100))[["cmt"]], adm_cmt(mod1))
  expect_equal(see_data(adm_lines(mod2, amt = 100))[["cmt"]], adm_cmt(mod2))
  expect_equal(see_data(adm_lines(mod3, amt = 100))[["cmt"]], adm_cmt(mod3))
})

test_that("explicit cmt works well",{
  expect_equal(see_data(adm_lines(mod1, amt = 100, cmt = 1))[["cmt"]], 1)
  expect_equal(see_data(adm_lines(mod1, amt = 100, cmt = c(3, -99)))[["cmt"]], c(-99, 3)) #arrange by cmt number !
})

test_that("rate incrementation is ok",{
  expect_equal(see_data(adm_lines(mod1, amt = 100))[c("cmt","rate")], tibble(cmt = c(1,2), rate = c(0, -2)))
  expect_equal(see_data(adm_lines(mod2, amt = 100))[c("cmt","rate")], tibble(cmt = 1, rate = -2))
  expect_null(see_data(adm_lines(mod3, amt = 100))[["rate"]])
})

test_that("rate incrementation is ok with explicit cmt",{
  expect_equal(see_data(adm_lines(mod2, amt = 100, cmt = 3))[["rate"]], 0)
  expect_equal(see_data(adm_lines(mod2, amt = 100, cmt = c(1, 3, -99)))[c("cmt","rate")], tibble(cmt = c(-99, 1, 3), rate = c(0, -2 , 0)))
})

test_that("rate incrementation is ok with explicit rate",{
  expect_equal(see_data(adm_lines(mod2, amt = 100, cmt = 3, rate = 150))[["rate"]], 150)
  expect_equal(see_data(adm_lines(mod2, amt = 100, cmt = c(1, 3, -99), rate = 150))[c("cmt","rate")], tibble(cmt = c(-99, 1, 3), rate = 150))
})

test_that("ID increment ok", {
  expect_equal((mod3 %>% adm_lines(ID = 3, amt = 100) %>% see_data())$ID, 3)
  expect_equal((mod3 %>% adm_lines(ID = 3, amt = 100) %>% obs_lines(time = 23, DV = 1.01) %>% see_data())$ID, c(3,3))
  expect_equal((mod3 %>% adm_lines(ID = 3, amt = 100) %>% adm_lines(ID = 2, time = 1, amt = 1) %>% see_data())$ID, c(2,3)) #sort by ID
  #Not ok for obslines... yet ? :
  expect_error(mod3 %>% adm_lines(ID = 1, amt = 100) %>% obs_lines(ID = 88))
})

test_that("realize addl works", {
  expect_equal(
    mod3 %>%
      adm_lines(amt = 100, addl = 9, ii = 24, realize_addl = T) %>%
      see_data() %>%
      nrow(),
    10
  )
})

