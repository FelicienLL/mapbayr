est001test <- mapbayest(exmodel(ID = 1:8), verbose = FALSE, progress = FALSE)

test_that("get_phi works", {
  phi001 <- get_phi(est001test)
  expect_equal(nrow(phi001), 8)
  expect_equal(phi001$SUBJECT_NO, 1:8)
  expect_equal(phi001$ID, 1:8)
  expect_named(phi001, c("SUBJECT_NO", "ID", "ETA1", "ETA2", "ETA3", "ETC1_1", "ETC2_1", "ETC2_2", "ETC3_1", "ETC3_2", "ETC3_3", "OBJ"))
})

test_that("no error if covariance does not exists", {
  # hessian = FALSE
  est001_nohessian <- est001test
  est001_nohessian$covariance[c(1,3,5,8)] <- matrix(NA_real_)
  phi_nohessian <- as.data.frame(get_phi(est001_nohessian))
  expect_equal(phi_nohessian[c(1,3,5,8), "ETC1_1"], rep(NA_real_, 4))

  # $covariance does not exists (mapbayr < 0.6.0)
  est001_old <- est001test
  est001_old$covariance <- NULL
  phi_old <- as.data.frame(get_phi(est001_old))
  expect_equal(phi_old$ETC1_1, rep(NA_real_, 8))
})

test_that("ETA is ET if n>9", {
  expect_equal(
    etanames_as_nonmem(c("ETA1", "ETA12", "ETC1", "ETC22")),
    c("ETA1", "ET12", "ETC1", "ETC22")
  )
})



