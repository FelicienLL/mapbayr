nmphi <- read_nmphi(system.file("nm001", "run001.phi", package = "mapbayr"))
merged <- merge_phi(mapbayr_phi = get_phi(est001), nonmem_phi = nmphi)

test_that("read_nmphi works", {
  expect_equal(nrow(nmphi), 8)
  expect_equal(nmphi$SUBJECT_NO, 1:8)
  expect_equal(nmphi$ID, 1:8)
  expect_named(nmphi, c("SUBJECT_NO", "ID", "ETA1", "ETA2", "ETA3", "ETC1_1", "ETC2_1", "ETC2_2", "ETC3_1", "ETC3_2", "ETC3_3", "OBJ"))
})

test_that("merge_phi works", {
  expect_equal(nrow(merged), 8*(3+6+1))
  expect_equal(unique(merged$type), c("ETA", "VARIANCE", "COVARIANCE", "OBJ"))
  expect_named(merged, c("SUBJECT_NO", "ID", "variable", "type", "mapbayr", "nonmem", "adiff"))

  expect_equal(merged$type[merged$variable=="ETA1"], rep("ETA", 8))
  expect_equal(merged$type[merged$variable=="ETA2"], rep("ETA", 8))
  expect_equal(merged$type[merged$variable=="ETA3"], rep("ETA", 8))

  expect_equal(merged$type[merged$variable=="ETC1_1"], rep("VARIANCE", 8))
  expect_equal(merged$type[merged$variable=="ETC2_2"], rep("VARIANCE", 8))
  expect_equal(merged$type[merged$variable=="ETC3_3"], rep("VARIANCE", 8))

  expect_equal(merged$type[merged$variable=="ETC2_1"], rep("COVARIANCE", 8))
  expect_equal(merged$type[merged$variable=="ETC3_1"], rep("COVARIANCE", 8))
  expect_equal(merged$type[merged$variable=="ETC3_2"], rep("COVARIANCE", 8))

  expect_equal(merged$type[merged$variable=="OBJ"], rep("OBJ", 8))

})

test_that("is.variance works", {
  v <- c("ETC11_1", "ETC11_11", "ETC1_1", "ETC1_11", "ETC2_11")
  expect_equal(is.variance(v), c(FALSE, TRUE, TRUE, FALSE, FALSE))
})

test_that("plot_phi works", {
  expect_s3_class(plot_phi(merged), "ggplot")
})
