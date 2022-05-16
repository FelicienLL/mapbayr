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
