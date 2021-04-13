test_that("hist.mapbayests works", {

  mod3 <- mread("ex_mbr3", mbrlib())
  data1 <- mod3 %>%
    adm_lines(amt = 100, rate = 100) %>%
    obs_lines(time = c(2, 4, 6), DV = c(8.1, 6.2, 4.3)) %>%
    get_data()

  data2 <- mod3 %>%
    adm_lines(amt = 100, rate = 30) %>%
    obs_lines(time = c(4, 6, 10), DV = c(21.1, 15.2, 10.3)) %>%
    get_data() %>%
    mutate(ID = 2)

  data12 <- bind_rows(data1, data2)

  est1 <- mapbayest(mod3, data1, verbose = F)
  est2 <- mapbayest(mod3, data2, verbose = F)
  est12 <- mapbayest(mod3, data12, verbose = F)
  hist1 <- hist(est1)
  hist2 <- hist(est2)
  hist12<- hist(est12)

  expect_s3_class(hist1$layers[[1]]$geom, "GeomArea")
  expect_s3_class(hist1$layers[[2]]$geom, "GeomLine")
  expect_s3_class(hist1$layers[[3]]$geom, "GeomSegment")
  expect_s3_class(hist1$layers[[4]]$geom, "GeomSegment")
  expect_s3_class(hist1$layers[[5]]$geom, "GeomRug")
  expect_s3_class(hist1$layers[[6]]$geom, "GeomBar")

  label1_1 <- eval(quote(pairlist(...)), envir = environment(hist1[["facet"]][["params"]][["labeller"]]))[["name"]][["ETA1"]]
  label2_1 <- eval(quote(pairlist(...)), envir = environment(hist2[["facet"]][["params"]][["labeller"]]))[["name"]][["ETA1"]]
  label12_1 <- eval(quote(pairlist(...)), envir = environment(hist12[["facet"]][["params"]][["labeller"]]))[["name"]][["ETA1"]]

  expect_true(str_detect(label1_1,  "ID percentile"))
  expect_true(str_detect(label2_1,  "ID percentile"))
  expect_true(!str_detect(label12_1, "ID percentile"))
})
