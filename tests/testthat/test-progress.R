test_that("progress bar works", {
  testthat::skip("cannot test progress bar")
# works manually, but not through automatic testing procedures
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
  my_data <- data.frame(ID = 1, TIME = c(0,12), EVID = c(1,0), AMT = c(500,0), CMT = c(1,2), DV = c(0, 10))
  my_data10 <- seq(10) %>%
    map_dfr(.f = ~ mutate(my_data, ID = .x))

  expect_message(mapbayest(x = my_model, my_data10), "\\[=====")
  expect_message(mapbayest(x = my_model, my_data10, progress = FALSE), NA)
})
