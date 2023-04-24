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

test_that("compute_weights works", {

  m0 <- matrix(c(0.8564, 0.9833, 0.1436, 0.0167), nrow = 2)

  m1 <- m0
  rownames(m1) <- c(2,9)
  expect_equal(compute_weights(est1, est6), m1, tolerance = 0.001)
  expect_equal(compute_weights(estlist = list(est1, est6)), m1, tolerance = 0.001)

  m2 <- m1
  colnames(m2) <- c("", "B")
  expect_equal(compute_weights(est1, B = est6), m2, tolerance = 0.001)
  expect_equal(compute_weights(estlist = list(est1, B = est6)), m2, tolerance = 0.001)

  expect_error(compute_weights("foo", est1, "bar"), "All objects passed to")
  expect_error(compute_weights(list(est1, B = est6)), "Did you forget")

  expect_equal(compute_weights(est1, est6, scheme = "AIC"),
               matrix(
                 c(0.9419, 0.9938, 0.0581, 0.00619),
                 nrow = 2,
                 dimnames = list(c("2", "9"), NULL)),
               tolerance = 0.001
  )

  est1bis <- est1
  est1bis$opt.value$ID <- c("2222", "9")
  expect_error(compute_weights(est1bis, est6), "Subject IDs are not the same")

  expect_message(ans <- compute_weights(est1, estlist = list(est1, est6)), "estlist not NULL")
  expect_equal(ans, m1, tolerance = 0.001)

  expect_equal(compute_weights(est1), matrix(c(1,1), dimnames = list(c("2", "9"), NULL)))
  est1ter <- est1
  est1ter$opt.value <- est1ter$opt.value[1,]
  expect_equal(compute_weights(est1ter), matrix(c(1), dimnames = list(c("2"), NULL)))
})


test_that("apply_weights() works", {
  #can average a data.frame
  tabs <- list(
    e1 = data.frame(ID = 1, time = c(0, 24), IPRED = c(100, 1000)),
    e2 = data.frame(ID = 1, time = c(0, 24), IPRED = c(200, 2000))
  )
  expect_equal(
    apply_weights(itabs = tabs, iweights = c(0.8, 0.2)),
    data.frame(ID = 1, time = c(0, 24), IPRED = c(120, 1200))
    # c(0.8*100 + 0.2*200, 0.8*1000 + 0.2*2000)
  )

  #can average a list of vectors
  expect_equal(
    apply_weights(itabs = list(c(100, 1000), c(200, 2000)), iweights = c(0.8, 0.2)),
    c(120, 1200)
  )

  #can average a single value
  expect_equal(
    apply_weights(itabs = c(100, 200), iweights = c(0.8, 0.2)),
    120
  )
})

test_that("do_model_averaging() works", {
  tabs <- list(
    A = data.frame(ID = c(1, 1, 2, 2),
                   time = c(0, 24, 0, 24),
                   IPRED = c(100, 200, 1000, 2000)),
    B = data.frame(ID = c(1, 1, 2, 2),
                   time = c(0, 24, 0, 24),
                   IPRED = c(80, 150, 900, 2200))
  )

  mat <- matrix(c(0.75, 0.9, 0.25, 0.1), nrow = 2,
                dimnames = list(c(1,2), c("A", "B")))

  expect_equal(
    do_model_averaging(list_of_tabs = tabs, weights_matrix = mat),
    data.frame(ID = c(1, 1, 2, 2), time = c(0, 24, 0, 24),
               IPRED = c(95, 187.5, 990, 2020))

  )

  # Even with non-numeric column #197

  Achar <- data.frame(ID = rep(1:2, each = 2), type = rep(c("PRED", "IPRED"), 2), num = as.double(1:4))
  Bchar <- mutate(Achar, num = as.double(5:8))

  expect_error(are_comparable(Achar, "foo")) # type of object
  expect_error(are_comparable(Achar, mutate(Bchar, foo = 1))) # number of variables
  expect_error(are_comparable(Achar, mutate(Bchar, ID = "foo"))) # type of variable
  expect_error(are_comparable(Achar, mutate(Bchar, type = "foo"))) # content of non-numeric variables

  expect_equal(
    do_model_averaging(list_of_tabs = list(A = Achar, B = Bchar),weights_matrix = mat),
    data.frame(ID = rep(1:2, each = 2), type = rep(c("PRED", "IPRED")), num = c(2, 3, 3.4, 4.4))
  )


})

test_that("model_averaging works", {
  expect_equal(
    model_averaging(est1, est6, output_function = ~select(filter(as.data.frame(.x), mdv == 0), ID, time, IPRED)),
    data.frame(ID = c(2, 9), time = c(96, 96), IPRED = c(0.938,0.961)),
    tolerance = 0.001
  )
})
