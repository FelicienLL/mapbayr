code_hist <- "
$PARAM
ETA1 = 0, ETA2 = 0, ETA3= 0, ETA4 = 0, ETA5 = 0, ETA6 = 0,
ETA7 = 0, ETA8 = 0, ETA9 = 0, ETA10 = 0, ETA11 = 0, ETA12 = 0
$OMEGA .1 .2 .3 .4 .1 .1 .1 .1 .1 .11 .11 .11
$CMT CENT GUT
$SIGMA 1 0
$TABLE
double DV = 100.0 ;
$CAPTURE DV
"

mod_hist <- mrgsolve::mcode("mod12", code_hist)
dat_hist <- exdata()
est_hist <- mapbayest(mod_hist, dat_hist, reset = 0, verbose = FALSE)
hist_hist <- hist(est_hist)

fetch_facet_names <- function(x){
  layout <- ggplot2::ggplot_build(x)$layout
  layout$layout[names(layout$facet$params$facets)]$name
}

fetch_facet_labels <- function(x){
  eval(quote(pairlist(...)), envir = environment(x[["facet"]][["params"]][["labeller"]]))
}

test_that("hist.mapbayests works", {
  expect_s3_class(hist_hist$layers[[1]]$geom, "GeomArea")
  expect_s3_class(hist_hist$layers[[2]]$geom, "GeomLine")
  expect_s3_class(hist_hist$layers[[3]]$geom, "GeomSegment")
  expect_s3_class(hist_hist$layers[[4]]$geom, "GeomSegment")
  expect_s3_class(hist_hist$layers[[5]]$geom, "GeomRug")
  expect_s3_class(hist_hist$layers[[6]]$geom, "GeomBar")
})

test_that("ID percentile is shown if n ID == 1", {
  label_hist <- fetch_facet_labels(hist_hist)[["name"]][["ETA1"]]
  expect_true(str_detect(label_hist, "ID percentile"))
})

test_that("SHK is shown if n ID > 1", {
  hist001 <- hist(est001)
  labelhist001 <- fetch_facet_labels(hist001)[["name"]]
  expect_true(all(!str_detect(labelhist001, "ID percentile")))

  # Default = based on SD
  expect_true(str_detect(labelhist001["ETA1"], "SHK = 23%"))

  # SD based
  hist_shk_sd <- hist(est001, shk = "sd")
  label_sd <- fetch_facet_labels(hist_shk_sd)[["name"]]
  expect_true(str_detect(label_sd["ETA1"], "SHK = 23%"))

  # VAR based
  hist_shk_var <- hist(est001, shk = "var")
  label_var <- fetch_facet_labels(hist_shk_var)[["name"]]
  expect_true(str_detect(label_var["ETA1"], "SHK = 40%"))

  # NA
  hist_shk_na <- hist(est001, shk = NA)
  label_na <- fetch_facet_labels(hist_shk_na)[["name"]]
  expect_equal(label_na["ETA1"], c(ETA1 = "CL\nIIV = 45%"))

})

test_that("ID percentile is not shown if n ID > 1", {
  label_hist <- eval(quote(pairlist(...)), envir = environment(hist_hist[["facet"]][["params"]][["labeller"]]))[["name"]][["ETA1"]]
  expect_true(str_detect(label_hist,  "ID percentile"))

  hist001 <- hist(est001)
  labelhist001 <- eval(quote(pairlist(...)), envir = environment(hist001[["facet"]][["params"]][["labeller"]]))[["name"]][["ETA1"]]
  expect_true(!str_detect(labelhist001, "ID percentile"))
})

test_that("facetting order is ok if nETA >= 10", {
  expected_names <- c("ETA1", "ETA2", "ETA3", "ETA4", "ETA5", "ETA6", "ETA7", "ETA8", "ETA9", "ETA10", "ETA11", "ETA12")
  expected_names <- factor(x = expected_names, levels = expected_names)
  expect_equal(fetch_facet_names(hist_hist), expected_names)
})

test_that("select_eta argument works", {
  expect_equal(
    fetch_facet_names(hist(est_hist, select_eta = c(1,3,5))),
    factor(x = make_eta_names(c(1,3,5)), levels = make_eta_names(c(1,3,5)))
  )

  # Even if estimation on a subset of ETAs
  est135 <- mapbayest(mod_hist, dat_hist, reset = 0, verbose = FALSE, select_eta = c(1,3,5))

  # Default to estimated ETAs
  expect_equal(
    fetch_facet_names(hist(est135)),
    factor(x = make_eta_names(c(1,3,5)), levels = make_eta_names(c(1,3,5)))
  )

  expect_equal(
    fetch_facet_names(hist(est135, select_eta = c(1,3))),
    factor(x = make_eta_names(c(1,3)), levels = make_eta_names(c(1,3)))
  )

  # Even if none are common
  expect_equal(
    fetch_facet_names(hist(est135, select_eta = c(2,4,6))),
    factor(x = make_eta_names(c(2,4,6)), levels = make_eta_names(c(2,4,6)))
  )

  # Error if select > max ETA
  expect_error(
    fetch_facet_names(hist(est135, select_eta = c(13, 14))),
    "Cannot select ETA13 ETA14: maximum 12 ETAs defined in \\$PARAM."
  )
})
