pchelle_code <- "$PROB
$PARAM ETA1 = 0, ETA2 = 0
$OMEGA 0.05 0.02 // CL and V
$SIGMA 0.1 0.5 // prop and add
$MAIN
double CL = 0.2 * exp(ETA(1) + ETA1) ;
double V1 = 2.0 * exp(ETA(2) + ETA2) ;
double Q = 0.1 ;
double V2 = 0.5;
$PKMODEL cmt='CENT GUT', depot = FALSE
$TABLE
capture DV = (CENT/V1) * (1 + EPS(1)) + EPS(2);"

pchelle_mod <- mcode("pchelle_mod", pchelle_code)

id1 <- adm_rows(cmt = 1, amt = 100, rate = 1200, LLOQ = 1) %>%
  obs_rows(cmt = 1, DV = c(75, 25, 7), time = c(1, 6, 24), BLQ = 0) %>%
  obs_rows(cmt = 1, DV = -1, time = 48, BLQ = 1)
pchelle_data <- bind_rows(id1, mutate(id1, ID = 2, BLQ = 0, DV = abs(DV)))

test_that("blq works on pchelle example", {
  est <- mapbayest(pchelle_mod, pchelle_data, output = "eta")
  expect_equal(unname(est), matrix(c(-4.27451E-02, -7.06898E-02, -1.22636E-01, -1.13914E-01), nrow = 2), tolerance = .0001)
})
