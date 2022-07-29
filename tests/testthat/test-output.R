test_that("mapbayests object `slots` are correct", {

  expect_named(est001, c("model", "arg.optim", "arg.ofv.fix", "arg.ofv.id", "opt.value", "final_eta", "covariance", "mapbay_tab", "information"))
  expect_named(est001$arg.ofv.fix, c("qmod", "sigma", "log_transformation", "omega_inv", "all_cmt"))
  expect_length(est001$arg.ofv.id, 8)
  expect_named(est001$arg.ofv.id[[1]], c("idvaliddata", "idDV", "idcmt"))
  expect_named(est001$arg.ofv.id[[2]], c("idvaliddata", "idDV", "idcmt"))

  expect_null(est001$data)

})

mod <- exmodel(301, add_exdata = FALSE, capture = "CL")
dat <- exdata(301) %>% mutate(FOO = 99) %>% filter(time < 72) %>% select(-SEX)
est1 <- mod %>%
  data_set(dat) %>% #data_set() function matters for the test, do not simplify
  mapbayest()

test_that("output tab is correct if single type of DV and covariate", {
  expect_names_1 <- c("ID", "time", "evid", "amt", "cmt", "ii", "addl", "mdv", "DV",
                      "BW", "FOO",
                      "SEX",
                      "CL", "IPRED", "PRED",
                      "ETA1", "ETA2", "ETA3")

  expect_named(as.data.frame(est1), expect_names_1, ignore.order = TRUE)

  expect_equal(as.data.frame(est1)[1,"SEX"], 0)
  expect_equal(as.data.frame(est1)[1,"BW"], 77)
  expect_equal(as.data.frame(est1)[1,"CL"], 5.08330165, tolerance = 0.0001)

})

test_that("output tab is correct if model with metabolite", {
  expect_names_2 <- c("ID", "time", "evid", "mdv", "amt", "ii", "addl", "cmt",
                      "DV", "PRED", "IPRED", "PAR", "MET",
                      paste0("ETA", 1:5))

  expect_named(mapbayest(exmodel(401), output = "df"), expect_names_2, ignore.order = TRUE)
})

test_that("dataset is not carried with model object", {
  expect_null(est1$model@args$data)
})

test_that("output = 'eta' works", {
  expect_mat <- matrix(c(-0.1453658, 0.02405846, -0.006188025, 0.2948274, -0.18212879, 0.011288384),
                       ncol = 3, nrow = 2, byrow = TRUE, dimnames = list(c(2,6), c("ETA1", "ETA2", "ETA3")))

  ans <- mapbayest(exmodel(ID = c(2, 6)), output = "eta")
  expect_equal(ans, expect_mat, tolerance = 0.001)
})
