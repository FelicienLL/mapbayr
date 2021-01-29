#test_that("read model specifications", {
#  mod1 <- mread("ex_mbr1", mbrlib())
#  expect_equal(mbr_drug(mod1), "Examplinib1")
#  expect_equal(mbr_model_ref(mod1), "XXX et al, J Pharmacokinet, 2020")
#  expect_equal(mbr_model_file(mod1), "ex_mbr1")
#  expect_equal(mbr_model_name(mod1), "Ex Mbr1")
#})

test_that("read cmts", {
  mod1 <- mread("ex_mbr1", mbrlib())
  mod2 <- mread("ex_mbr2", mbrlib())
  mod3 <- mread("ex_mbr3", mbrlib())

  expect_equal(adm_cmt(mod1), c(1,2))
  expect_equal(adm_cmt(mod2), 1)
  expect_equal(adm_cmt(mod3), 1)

  expect_equal(obs_cmt(mod1), 2)
  expect_equal(obs_cmt(mod2), c(2,4))
  expect_equal(obs_cmt(mod3), 1)
})



