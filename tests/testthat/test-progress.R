code1 <- "$PARAM ETA1 = 0, ETA2 = 0, KA = 0.5, TVCL = 1.1, TVV = 23.3
 $OMEGA 0.41 0.32
 $SIGMA 0.04 0
 $CMT DEPOT CENT
 $PK
 double CL=TVCL*exp(ETA1+ETA(1));
 double V=TVV*exp(ETA2+ETA(2)) ;
 $ERROR
 double DV=CENT/V*(1+EPS(1))+EPS(2);
 $PKMODEL ncmt = 1, depot = TRUE
 $CAPTURE DV CL"
my_model <- mrgsolve::mcode("my_model", code1)
my_data <- data.frame(ID = 1, time = c(0,12), evid = c(1,0), mdv = c(1,0), amt = c(500,0), cmt = c(1,2), DV = c(0, 10))

test_that("progress argument works", {
  testthat::skip("cannot test progress bar")
  # works manually, but not through automatic testing procedures

  my_data10 <- seq(10) %>%
    map_dfr(.f = ~ mutate(my_data, ID = .x))

  expect_message(mapbayest(x = my_model, my_data10), "\\[=====")
  expect_message(mapbayest(x = my_model, my_data10, progress = FALSE), NA)
})

test_that("do_optimization works outside the call of mapbayest", {
  arg.ofv <- c(preprocess.ofv.fix(x = my_model, data = my_data), preprocess.ofv.id(x = my_model, iddata = my_data))
  arg.optim <- preprocess.optim(x = my_model, method = "L-BFGS-B", control = list(), force_initial_eta = NULL, quantile_bound = 0.001)

  expect_error(do_optimization(arg.ofv, arg.optim, verbose = F, reset = T), NA)
})
