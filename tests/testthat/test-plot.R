est <- mapbayest(exmodel())
test_that("plot works", {
  expect_message(p3 <- plot(est), NA)
  expect_s3_class(p3, "ggplot")
})

test_that("no warning if DV is NA and mdv = 1", {
  # Fix 114
  est$mapbay_tab[5, "mdv"] <- 1
  est$mapbay_tab[5, "DV"] <- NA
  expect_warning(plot(est), NA)
})

test_that("can choose type of prediction", {
  #112
  est1 <- exmodel(ID = 1) %>% mapbayest()
  expect_s3_class(plot(est1), "ggplot")
  expect_s3_class(plot(est1, PREDICTION = "IPRED"), "ggplot")
  expect_s3_class(plot(est1, PREDICTION = "PRED"), "ggplot")
})

test_that("reframe_observation() works", {

  data1 <- obs_rows(time = 0, DV = c(0.12, 1.2), cmt = c(1, 2)) %>%
    obs_rows(time = 24, DV = c(0.34, 3.4), cmt = c(1, 2), evid = 2) %>%
    obs_rows(time = 48, DV = c(0.56, 5.6), cmt = c(1, 2), mdv = c(0,1)) %>%
    obs_rows(ID = 2, time = 0, DV = c(0.78, 7.8), cmt = c(1,2)) %>%
    adm_rows(time = 24, amt = 100, cmt = 1)

  ref1 <- reframe_observations(data1)
  expect_equal(nrow(ref1), 8)
  expect_equal(unique(ref1$evid), c(0,2))
  expect_equal(levels(ref1$MDV), c("0","1"))
  expect_equal(levels(ref1$name), c("DV","PAR", "MET"))
  expect_equal(as.character(ref1$name), rep(c("PAR", "MET"), 4))
  expect_equal(data1$DV[1:8], ref1$value)

  ref2 <- reframe_observations(data1[1,])
  expect_equal(as.character(ref2$name), "DV")

  ref3 <- reframe_observations(data1[1,], predictions_names = c("PAR", "MET"))
  expect_equal(as.character(ref3$name), "PAR")
})


# est <- mapbayest(exmodel(401))
# au <- augment(est)$aug_tab
# au2 <- au %>% mutate(value = value * 2, cmt = cmt+1)
# auau <- bind_rows(MACHIN = au, BIDULE = au2, .id = "MODEL")
# OBS <- as.data.frame(est) %>% filter(evid == 0)
# OBS[5,"mdv"] <- 1
# auau <- bind_rows(auau, mutate(auau, ID = 2, value = value+50))
#
# mapbayr_plot(mutate(au, MODEL = "trucmuche"), OBS)
# mapbayr_plot(auau, OBS)
# mapbayr_plot(auau, OBS, PREDICTION = "PRED")
# mapbayr_plot(auau, OBS, MODEL_color = c(MACHIN = "black", BIDULE = "blue"))
#
# model_coloration(model_names = c("Yu", "Imbs 2014", "Imbs2016", "Average", "truc"))
# model_coloration(
#   model_names = c("Yu", "Imbs 2014", "Imbs2016", "Average", "truc"),
#   forced_colorations = c(Average = "black")
# )
