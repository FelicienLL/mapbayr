test_that("MDV == 0 are handled properly", {
  mod <- exmodel(301, FALSE)
  dat <- exdata(301) %>%
    filter(time < 72) %>%
    mutate(BW = 75, SEX = 0) #base data, 4 conc to fit

  dat <- bind_rows(
    dat,
    dat %>%
      mutate(ID = 2,            # ID2 =
             mdv = c(1,1,0,0,0) # 3 conc to fit instead of 4
      ),
    dat %>%
      mutate(ID = 3,                 # ID 3 =
             mdv = c(1,1,0,0,0),     # Still one conc ignored, but new info on CL:
             BW = c(75, 100, 75, 75, 75) # the line must be read by mrgsim, but not for OFV computation
      ),
    dat %>%
      mutate(ID = 4,                  #ID 4
             mdv = c(1,1,0,0,0),      # the line is just dropped
             BW = c(75, 100, 75, 75, 75)
      ) %>%
      filter(BW != 100)
  )

  est <- mapbayest(mod, dat)

  #Test number of observation fitted
  expect_equal(length(est$arg.ofv.id[[1]]$idDV), 4)
  expect_equal(length(est$arg.ofv.id[[2]]$idDV), 3)
  expect_equal(length(est$arg.ofv.id[[3]]$idDV), 3)
  expect_equal(length(est$arg.ofv.id[[4]]$idDV), 3)

  #Test different eta are found
  # 1 != 2
  # 1 != 3
  # 1 != 4
  # 2 != 3
  # 2 == 4
  # 3 != 4

  eta <- get_eta(est, output = 'list')

  expect_false(isTRUE(all.equal(eta[[1]], eta[[2]])))
  expect_false(isTRUE(all.equal(eta[[1]], eta[[3]])))
  expect_false(isTRUE(all.equal(eta[[1]], eta[[4]])))
  expect_false(isTRUE(all.equal(eta[[2]], eta[[3]])))
  expect_true (isTRUE(all.equal(eta[[2]], eta[[4]])))
  expect_false(isTRUE(all.equal(eta[[3]], eta[[4]])))
})
