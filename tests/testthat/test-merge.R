datadf <- exdata(1, c(1,4))
datamatrix <- as.matrix(datadf)
etamatrix <- get_eta(est001, output = "num")[c(1,4),]
etavec <- eta(runif(3))

test_that("merge_datadf_etavec() works", {
  merge_dfvec <- merge_datadf_etavec(datadf, etavec)
  expect_s3_class(merge_dfvec, "data.frame")
  expect_named(merge_dfvec, c(names(datadf), names(etavec)))
})

test_that("merge_datadf_etamatrix works", {
  merged_dfmat <- merge_datadf_etamatrix(data_df = datadf, eta_matrix = etamatrix)
  expect_equal(merged_dfmat[["ETA1"]], c(rep(etamatrix[1, "ETA1"], 5), rep(etamatrix[2, "ETA1"], 3)))
})

test_that("merge_datamatrix_etamatrix works", {
  merged_matmat <- merge_datamatrix_etamatrix(data_matrix = datamatrix, eta_matrix = etamatrix)
  expect_equal(
    unname(merged_matmat[,"ETA1"]),
    c(rep(etamatrix[1, "ETA1"], 5), rep(etamatrix[2, "ETA1"], 3))
  )

  expect_error(
    merge_datamatrix_etamatrix(data_matrix = datamatrix[1:5,], eta_matrix = etamatrix),
    "Number of subjects in data is not the number of subjects in 'ETA' matrix"
  )

  etamatrix2 <- etamatrix
  rownames(etamatrix2) <- NULL
  merged_matmat2 <- merge_datamatrix_etamatrix(data_matrix = datamatrix, eta_matrix = etamatrix2)

  expect_equal(merged_matmat, merged_matmat2)
})

test_that("replicate_data works", {
  expect_equal(
    replicate_data(data.frame(ID = c(1,1,2,3,3,3), time = c(0,24,0,0,24,48)), n = 2),
    matrix(c(
      c(1,1,2,3,3,3,4,4,5,6,6,6),
      c(0,24,0,0,24,48,0,24,0,0,24,48)
    ), ncol = 2, dimnames = list(NULL, c("ID","time"))
    )
  )
})
