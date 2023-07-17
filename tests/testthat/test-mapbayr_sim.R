MOD <- exmodel(401, add_exdata = FALSE)
DAT <- exdata(401, 1:2)
EST <- mapbayest(MOD, DAT, verbose = FALSE, progress = FALSE)
ETA <- get_eta(EST, output = "num")

DAT1 <- exdata(401, 1)
EST1 <- mapbayest(MOD, DAT1, verbose = FALSE, progress = FALSE)
ETA1 <- get_eta(EST1, output = "num")
nonmem_pred  <- c(105.262, 4.564, 115.971, 18.985)
nonmem_ipred <- c(92.380, 4.227, 98.689, 17.745)

test_that("do_mapbayr_sim() works", {

  sim001 <- do_mapbayr_sim(zero_re(MOD), DAT, obsaug = FALSE, nrep = NULL, eta = NULL)
  expect_equal(nrow(sim001), nrow(DAT))
  expect_equal(sim001$ID, DAT$ID)
  expect_equal(sim001$DV[sim001$time %in% c(1.5, 4.4)], nonmem_pred, tolerance = 0.001)

  sim001b <- do_mapbayr_sim(MOD, DAT, obsaug = FALSE, nrep = NULL, eta = NULL, new_omega = "zero_re", new_sigma = "zero_re")
  expect_equal(sim001, sim001b)

  sim002 <- do_mapbayr_sim(zero_re(MOD), DAT, obsaug = FALSE, nrep = NULL, eta = ETA)
  expect_equal(nrow(sim002), nrow(DAT))
  expect_equal(sim002$ID, DAT$ID)
  expect_equal(sim002$DV[sim002$time %in% c(1.5, 4.4)], nonmem_ipred, tolerance = 0.001)

  sim003 <- do_mapbayr_sim(MOD, DAT, obsaug = FALSE, nrep = 10, eta = NULL)
  expect_equal(nrow(sim003), nrow(DAT) * 10)

  sim004 <- do_mapbayr_sim(MOD, DAT, obsaug = FALSE, nrep = 10, eta = ETA)
  expect_equal(nrow(sim004), nrow(DAT) * 10)
  expect_equal(unique(sim004$ID), seq_len(10 * 2))
  expect_equal(sim004$time[sim004$ID == 1], sim004$time[sim004$ID == 3])
  expect_true(all(sim004$DV[sim004$ID == 1 & sim004$time>0] != sim004$DV[sim004$ID == 3 & sim004$time>0]))


  sim005 <- do_mapbayr_sim(zero_re(MOD), DAT, obsaug = TRUE, nrep = NULL, eta = NULL)
  expect_true(all(c(1,2,3,4) %in% sim005$time))
  expect_equal(sim005$DV[sim005$time %in% c(1.5, 4.4)], nonmem_pred, tolerance = 0.001)

  sim006 <- do_mapbayr_sim(zero_re(MOD), DAT, obsaug = TRUE, nrep = NULL, eta = ETA)
  expect_true(all(c(1,2,3,4) %in% sim006$time))
  expect_equal(sim006$DV[sim006$time %in% c(1.5, 4.4)], nonmem_ipred, tolerance = 0.001)

  set.seed(123)
  sim007a <- do_mapbayr_sim(MOD, DAT, obsaug = TRUE, nrep = 10, eta = NULL)
  set.seed(123)
  sim007b <- do_mapbayr_sim(MOD, DAT, obsaug = TRUE, nrep = 10, eta = NULL)
  set.seed(456)
  sim007c <- do_mapbayr_sim(MOD, DAT, obsaug = TRUE, nrep = 10, eta = NULL)

  expect_equal(sim007a, sim007b)
  expect_false(all(sim007a$DV == sim007c$DV))

  set.seed(123)
  sim008a <- do_mapbayr_sim(MOD, DAT, obsaug = TRUE, nrep = 10, eta = ETA)
  set.seed(123)
  sim008b <- do_mapbayr_sim(MOD, DAT, obsaug = TRUE, nrep = 10, eta = ETA)
  set.seed(456)
  sim008c <- do_mapbayr_sim(MOD, DAT, obsaug = TRUE, nrep = 10, eta = ETA)

  expect_equal(sim008a, sim008b)
  expect_false(all(sim008a$DV == sim008c$DV))

  expect_false(all(sim007a$DV == sim008a$DV))
  expect_equal(nrow(sim008a), nrow(sim005) * 10)


})

