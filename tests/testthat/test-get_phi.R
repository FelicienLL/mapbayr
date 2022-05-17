test_that("get_phi works", {
  phi001 <- get_phi(est001)
  expect_equal(nrow(phi001), 8)
  expect_equal(phi001$SUBJECT_NO, 1:8)
  expect_equal(phi001$ID, 1:8)
  expect_named(phi001, c("SUBJECT_NO", "ID", "ETA1", "ETA2", "ETA3", "ETC1_1", "ETC2_1", "ETC2_2", "ETC3_1", "ETC3_2", "ETC3_3", "OBJ"))
})
