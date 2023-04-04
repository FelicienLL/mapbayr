same_data_and_est <- function(x){
  x %>%
    adm_rows(ID = 2, time = 0, amt = 100, addl = 3, ii = 24) %>%
    obs_rows(ID = 2, time = 96, DV = 1) %>%
    adm_rows(ID = 9, time = 0, amt = 200, addl = 3, ii = 24) %>%
    obs_rows(ID = 9, time = 96, DV = 1) %>%
    mapbayest()
}

est1 <- exmodel(1, add_exdata = F) %>%
  param(TVCL = 4) %>%
  same_data_and_est()

est6 <- exmodel(6, add_exdata = F) %>%
  param(TVCL = 1) %>%
  same_data_and_est()


test_that("get_LL works", {
  expect_equal(
    get_LL(est1),
    matrix(c(3.294, 4.565), dimnames = list(c(2,9), NULL)),
    tolerance = 0.001
  )

  expect_equal(
    get_LL(est1, LL = FALSE),
    matrix(c(-2.384, -3.037), dimnames = list(c(2,9), NULL)),
    tolerance = 0.001
  )
})

test_that("get_AIC works", {
  expect_equal(
    get_AIC(est1),
    matrix(c(0.164, 0.227), dimnames = list(c(2,9), NULL)),
    tolerance = 0.001
  )
})

test_that("model_averaging works", {

  m0 <- matrix(c(0.8564, 0.9833, 0.1436, 0.0167), nrow = 2)

  m1 <- m0
  rownames(m1) <- c(2,9)
  expect_equal(model_averaging(est1, est6), m1, tolerance = 0.001)
  expect_equal(model_averaging(estlist = list(est1, est6)), m1, tolerance = 0.001)

  m2 <- m1
  colnames(m2) <- c("", "B")
  expect_equal(model_averaging(est1, B = est6), m2, tolerance = 0.001)
  expect_equal(model_averaging(estlist = list(est1, B = est6)), m2, tolerance = 0.001)

  expect_error(model_averaging("foo", est1, "bar"), "All objects passed to")
  expect_error(model_averaging(list(est1, B = est6)), "Did you forget")

  expect_equal(model_averaging(est1, est6, scheme = "AIC"),
               matrix(
                 c(0.9419, 0.9938, 0.0581, 0.00619),
                 nrow = 2,
                 dimnames = list(c("2", "9"), NULL)),
               tolerance = 0.001
  )

  est1bis <- est1
  est1bis$opt.value$ID <- c("2222", "9")
  expect_error(model_averaging(est1bis, est6), "Subject IDs are not the same")

  expect_message(ans <- model_averaging(est1, estlist = list(est1, est6)), "estlist not NULL")
  expect_equal(ans, m1, tolerance = 0.001)

  expect_equal(model_averaging(est1), matrix(c(1,1), dimnames = list(c("2", "9"), NULL)))
  est1ter <- est1
  est1ter$opt.value <- est1ter$opt.value[1,]
  expect_equal(model_averaging(est1ter), matrix(c(1), dimnames = list(c("2"), NULL)))
})






