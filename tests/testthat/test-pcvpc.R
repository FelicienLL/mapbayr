test_that("vpc_sim() works",{
  expect_s3_class(exmodel(1,   ID = 1:2) %>% mapbayr_vpc(nrep = 10), "ggplot")
  expect_s3_class(exmodel(301, ID = 1:2) %>% mapbayr_vpc(nrep = 10), "ggplot")
  expect_s3_class(exmodel(401, ID = 1:2) %>% mapbayr_vpc(nrep = 10), "ggplot")
})

test_that("vpc_sim() works", {
  library(mrgsolve)
  mod <- house() %>%
    omat(mrgsolve::dmat(rep(0.2,4)))

  # Creating dataset for the example
  # Same concentration, but different dose (ID 2) and covariate (ID 3)
  data <- adm_rows(ID = 1, amt = 1000, cmt = 1, addl = 6, ii = 12) %>%
    obs_rows(DV = 50, cmt = 2, time = 7 * 12) %>%
    adm_rows(ID = 2, time = 0, amt = 2000, cmt = 1, addl = 6, ii = 12) %>%
    obs_rows(DV = 50, cmt = 2, time = 7 * 12) %>%
    adm_rows(ID = 3, time = 0, amt = 1000, cmt = 1, addl = 6, ii = 12) %>%
    obs_rows(DV = 50, cmt = 2, time = 7 * 12) %>%
    add_covariates(SEX = c(0,0,0,0,1,1))
  vpcsim1 <- vpc_sim(mod, data, nrep = 10)
  expect_type(vpcsim1, "list")
  expect_named(vpcsim1, c("SIMTAB", "OBSTAB", "stratify_on", "idv"))

  expect_s3_class(vpcsim1$SIMTAB, "data.frame")
  expect_s3_class(vpcsim1$OBSTAB, "data.frame")
  expect_null(vpcsim1$stratify_on)
  expect_equal(vpcsim1$idv, "time")

  # pcvpc works
  expect_equal(vpcsim1$OBSTAB$value, c(73.84257, 36.92129, 50.00000), tolerance = 0.0001)

  # idv = time, by default
  expect_equal(vpcsim1$SIMTAB$time, vpcsim1$SIMTAB$idv)

  vpcsim2 <- vpc_sim(mod, data, nrep = 20, idv = "tad", pcvpc = FALSE)
  expect_equal(vpcsim2$OBSTAB$value, c(50, 50, 50))
  expect_equal(nrow(vpcsim2$SIMTAB), nrow(vpcsim1$SIMTAB)*2)
  expect_equal(unique(vpcsim2$SIMTAB$idv), seq(0,28,1))
  expect_equal(vpcsim2$OBSTAB$value, c(50, 50, 50))

  vpcsim3 <- vpc_sim(mod, data, nrep = 10, stratify_on = "SEX")
  expect_equal(vpcsim3$stratify_on, "SEX")
  expect_equal(vpcsim3$OBSTAB$value, c(75, 37.5, 50))
  expect_length(intersect(
    vpcsim3$SIMTAB$bin[vpcsim3$SIMTAB$SEX == 0],
    vpcsim3$SIMTAB$bin[vpcsim3$SIMTAB$SEX == 1]
  ), 0)

  vpcsim4 <- vpc_sim(mod, data, nrep = 10, start = 72, end = 96, delta = 0.1)
  expect_equal(unique(vpcsim4$SIMTAB$time), seq(72, 96, 0.1))

  data$char <- "foo"
  expect_error(
    vpc_sim(mod, data, stratify_on = "char", nrep = 10),
    "Variables defined with `stratify_on` are not all numeric"
  )

})
