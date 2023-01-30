test_that("hist.mapbayests works", {
  hist1 <- hist(mapbayest(exmodel()))
  hist001 <- hist(est001)

  expect_s3_class(hist1$layers[[1]]$geom, "GeomArea")
  expect_s3_class(hist1$layers[[2]]$geom, "GeomLine")
  expect_s3_class(hist1$layers[[3]]$geom, "GeomSegment")
  expect_s3_class(hist1$layers[[4]]$geom, "GeomSegment")
  expect_s3_class(hist1$layers[[5]]$geom, "GeomRug")
  expect_s3_class(hist1$layers[[6]]$geom, "GeomBar")

  labelhist1 <- eval(quote(pairlist(...)), envir = environment(hist1[["facet"]][["params"]][["labeller"]]))[["name"]][["ETA1"]]
  labelhist001 <- eval(quote(pairlist(...)), envir = environment(hist001[["facet"]][["params"]][["labeller"]]))[["name"]][["ETA1"]]

  expect_true(str_detect(labelhist1,  "ID percentile"))
  expect_true(!str_detect(labelhist001, "ID percentile"))
})

test_that("facetting order is ok if nETA >= 10", {
  code <- "
$PARAM ETA1 = 0, ETA2 = 0, ETA3= 0, ETA4 = 0, ETA5 = 0,
ETA6 = 0, ETA7 = 0, ETA8 = 0, ETA9 = 0, ETA10 = 0, ETA11 = 0, ETA12 = 0
$OMEGA .1 .2 .3 .4 .1 .1 .1 .1 .1 .11 .11 .11
$CMT CENT GUT
$SIGMA 1 0
$TABLE
double DV = 100.0 ;
$CAPTURE DV
"
  mod <- mrgsolve::mcode("mod", code)
  dat <- exdata()
  est <- mapbayest(mod, dat, reset = 0, verbose = FALSE)
  H <- hist(est)
  layout <- ggplot2::ggplot_build(H)$layout
  test_names <- layout$layout[names(layout$facet$params$facets)]$name
  expected_names <- make_eta_names(n = 12)
  expect_equal(levels(test_names), expected_names)
  expect_equal(as.character(test_names), expected_names)
})
