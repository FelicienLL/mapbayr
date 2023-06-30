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
