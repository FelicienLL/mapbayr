mod <- exmodel(add_exdata = FALSE)
dat <- exdata(ID = 1:3)
dat$ID[dat$ID == 2] <- 20
est_all <- mapbayest(mod, dat)

test_that("mapbayr fits multiple ID", {
  expect_equal(nrow(get_eta(est_all)), 3)
  esttab <- as.data.frame(est_all)
  expect_equal(dat$ID,   esttab$ID)
  expect_equal(dat$time, esttab$time  )
  expect_equal(dat$evid, esttab$evid)
  expect_equal(dat$amt,  esttab$amt)
  expect_equal(dat$cmt,  esttab$cmt)
  expect_equal(dat$mdv,  esttab$mdv)
  expect_equal(dat$DV,   esttab$DV)
})

test_that("order of IDs is preserved", {
  dat$ID[dat$ID == 2] <- 20

  est1 <- mapbayest(mod, dat[dat$ID == 1,])
  est20<- mapbayest(mod, dat[dat$ID == 20,])
  est3 <- mapbayest(mod, dat[dat$ID == 3,])

  expect_named(etalist <- get_eta(est_all, output = "list"), c("1", "20", "3"))
  expect_equal(
    lapply(list(est1, est20, est3), get_eta),
    unname(etalist)
  )
})
