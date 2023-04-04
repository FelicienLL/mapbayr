same_data <- function(x){
  x %>%
    adm_rows(ID = 2, time = 0, amt = 100, addl = 3, ii = 24) %>%
    obs_rows(ID = 2, time = 96, DV = 1) %>%
    adm_rows(ID = 9, time = 0, amt = 200, addl = 3, ii = 24) %>%
    obs_rows(ID = 9, time = 96, DV = 1)
}

est1 <- exmodel(1, add_exdata = F) %>%
  param(TVCL = 4) %>%
  same_data() %>%
  mapbayest()

est6 <- exmodel(6, add_exdata = F) %>%
  param(TVCL = 1) %>%
  same_data() %>%
  mapbayest()

est401 <- exmodel(401, add_exdata = F) %>%
  param(TVCL = 10) %>%
  same_data() %>%
  mapbayest()

test_that("model_averaging works", {

  m0 <- matrix(c(0.8564, 0.9833, 0.1436, 0.0167), nrow = 2)

  m1 <- m0
  rownames(m1) <- c(2,9)
  expect_equal(model_averaging(est1, est6), m1, tolerance = 0.001)
  expect_equal(model_averaging(modlist = list(est1, est6)), m1, tolerance = 0.001)

  m2 <- m1
  colnames(m2) <- c("", "B")
  expect_equal(model_averaging(est1, B = est6), m2, tolerance = 0.001)
  expect_equal(model_averaging(modlist = list(est1, B = est6)), m2, tolerance = 0.001)

  expect_error(model_averaging("foo", est1, "bar"), "All objects passed to")
  expect_error(model_averaging(list(est1, B = est6)), "Did you forget")
})






