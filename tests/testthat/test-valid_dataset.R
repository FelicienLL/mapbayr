mod1 <- mread('ex_mbr1', mbrlib())
data1 <- mod1 %>%
  adm_lines(time = 0, amt = 100) %>%
  obs_lines(time = 20, DV = 1.5) %>%
  see_data()
data1
test_that("Dataset check is correct", {
  expect_error(mbrest(mod1, data = select(data1, -ID)), "ID column is missing")
  expect_error(mbrest(mod1, data = select(data1, -time)), "time column is missing")
  expect_error(mbrest(mod1, data = select(data1, -evid)), "evid column is missing")
  expect_error(mbrest(mod1, data = select(data1, -cmt)), "cmt column is missing")
  expect_error(mbrest(mod1, data = select(data1, -amt)), "amt column is missing")


})

test_that("auto-supply of MDV if missing", {
  data_mdv <- select(data1, -mdv)
  expect_error(est <- mbrest(mod1, data_mdv), NA)
  expect_true(!is.null(est$mapbay_tab[["mdv"]]))
})


test_that("misspecification of evid/mdv are checked", {
  expect_error(mbrest(mod1, data = mutate(data1, evid = c(1,1,2))), "Lines with evid = 2 & mdv = 0 are not allowed")
  data2 <- mod1 %>%
    data_set(data1) %>%
    obs_lines(time = c(25,30), DV = c(1, .5)) %>%
    see_data() %>%
    mutate(evid = c(1, 1, 1, 3, 4))
  expect_error(mbrest(mod1, data = data2), "Lines with mdv = 0 must have evid = 0.")

  data3 <- mod1 %>%
    data_set(data1) %>%
    obs_lines(time = 0, DV = .005) %>%
    see_data()

  expect_error(mbrest(mod1, data3))
})
