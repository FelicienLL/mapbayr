test_that("plot works", {
  est <- mapbayest(exmodel())
  expect_message(p3 <- plot(est), NA)
  expect_s3_class(p3, "ggplot")
})
