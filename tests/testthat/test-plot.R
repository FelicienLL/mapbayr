est <- mapbayest(exmodel())
test_that("plot works", {
  expect_message(p3 <- plot(est), NA)
  expect_s3_class(p3, "ggplot")
})

test_that("no warning if DV is NA and mdv = 1", {
  # Fix 114
  est2 <- est
  est2$mapbay_tab[5, "mdv"] <- 1
  est2$mapbay_tab[5, "DV"] <- NA
  expect_warning(plot(est2), NA)
})

test_that("can choose type of prediction", {
  # Fix 112
  p_PREDIPRED <- plot(est)
  expect_s3_class(p_PREDIPRED, "ggplot")
  expect_equal(unique(p_PREDIPRED$data$PREDICTION), c("IPRED", "PRED"))

  p_PRED <- plot(est, PREDICTION = "PRED")
  expect_s3_class(p_PRED, "ggplot")
  expect_equal(unique(p_PRED$data$PREDICTION), "PRED")

  p_IPRED <- plot(est, PREDICTION = "IPRED")
  expect_s3_class(p_IPRED, "ggplot")
  expect_equal(unique(p_IPRED$data$PREDICTION), "IPRED")
})

test_that("reframe_observation() works", {
  data1 <- obs_rows(time = 0, DV = c(0.12, 1.2), cmt = c(1, 2)) %>%
    obs_rows(time = 24, DV = c(0.34, NA), cmt = c(1, 2), evid = 2) %>%
    obs_rows(time = 48, DV = c(0.56, 5.6), cmt = c(1, 2), mdv = c(0,1)) %>%
    obs_rows(ID = 2, time = 0, DV = c(0.78, 7.8), cmt = c(1,2)) %>%
    adm_rows(time = 24, amt = 100, cmt = 1)

  ref1 <- reframe_observations(data1)
  expect_equal(nrow(ref1), 8-1)
  expect_equal(unique(ref1$evid), c(0,2))
  expect_equal(levels(ref1$MDV), c("0","1"))
  expect_equal(levels(ref1$name), c("DV","PAR", "MET"))
  expect_equal(as.character(ref1$name), rep(c("PAR", "MET"), 4)[-4])
  expect_equal(ref1$value,data1$DV[c(1:3,5:8)])

  ref2 <- reframe_observations(data1[1,])
  expect_equal(as.character(ref2$name), "DV")

  ref3 <- reframe_observations(data1[1,], predictions_names = c("PAR", "MET"))
  expect_equal(as.character(ref3$name), "PAR")

})

test_that("mapbayr_plot works", {
  aug <- data.frame(
    ID = 1, name = factor("DV"), cmt = 2, time = rep(c(0,8,16,24), each = 2),
    type = rep(c("PRED", "IPRED"), 4), value = c(0, 0, 1, 2, 4, 8, 2, 4)
  )

  obs <- data.frame(
    ID = 1, time = c(6, 20), evid = 0,
    mdv = c(0,1), DV = c(0.5, 5), cmt = 2
  )

  # with no obs

  expect_true(
    any(unlist(sapply(map(mapbayr_plot(aug, obs)$layers, "geom"), class)) == "GeomPoint")
  )
  expect_false(
    any(unlist(sapply(map(mapbayr_plot(aug, obs = NULL)$layers, "geom"), class)) == "GeomPoint")
  )

  # Multimodel

  aug2 <- dplyr::bind_rows(
    FOO = aug,
    BAZ = dplyr::mutate(aug, value = value * 2),
    BAR = dplyr::mutate(aug, value = value * 3),
    .id = "MODEL"
  )

  expect_s3_class(mapbayr_plot(aug2, PREDICTION = "IPRED"), "ggplot")
  expect_s3_class(mapbayr_plot(aug2, PREDICTION = "IPRED",
                               MODEL_color = c(BAZ = "pink",
                                               BAR = "yellow",
                                               FLL = "red")), # does not exist
                  "ggplot")

})
