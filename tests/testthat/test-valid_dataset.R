mod1 <- mread('ex_mbr1', mbrlib())
data1 <- mod1 %>%
  adm_lines(time = 0, amt = 100) %>%
  obs_lines(time = 20, DV = 1.5) %>%
  get_data()

test_that("NM-TRAN items are specified", {
  expect_error(mapbayest(mod1, data = select(data1, -ID, -time)), "Missing column: ID time")
  expect_error(mapbayest(mod1, data = select(data1, -time, -evid, -cmt, -amt)), "Missing column: time evid cmt amt")
})

test_that("stops if non-numeric columns", {
  expect_error(mapbayest(mod1, data = mutate(data1, hello = "world", foo = "bar")), "Non-numeric column found: hello foo")
})

test_that("auto-supply of MDV if missing", {
  data_mdv <- select(data1, -mdv)
  expect_error(est <- mapbayest(mod1, data_mdv, verbose = F), NA)
  expect_true(!is.null(est$mapbay_tab[["mdv"]]))
})


test_that("misspecification of evid/mdv are checked", {
  expect_error(mapbayest(mod1, data = mutate(data1, evid = c(1,1,2))), "Lines with evid = 2 & mdv = 0 are not allowed")
  data2 <- mod1 %>%
    data_set(data1) %>%
    obs_lines(time = c(25,30), DV = c(1, .5)) %>%
    get_data() %>%
    mutate(evid = c(1, 1, 1, 3, 4))
  expect_error(mapbayest(mod1, data = data2), "Lines with mdv = 0 must have evid = 0.")

  data3 <- mod1 %>%
    data_set(data1) %>%
    obs_lines(time = 0, DV = .005) %>%
    get_data()

  expect_error(mapbayest(mod1, data3))
})


