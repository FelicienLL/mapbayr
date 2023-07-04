datadf <- exdata(1, c(1,4))
datamatrix <- as.matrix(datadf)
etamatrix <- get_eta(est001, output = "num")[c(1,4),]

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
  repdat <- replicate_data(data = datadf, nrep = 3)
  expect_true(inherits(repdat, "matrix"))
  expect_equal(unique(repdat[,"ID"]), as.double(1:6))
})
