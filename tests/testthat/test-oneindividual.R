#test_that("one individual", {
#  ex3 <- mread("ex_mbr3", mbrlib())
#  data1 <- (adm_lines(ex3, amt = 100))@args$data
#  data2 <- mutate(data1, ID = 2)
#  data12 <- bind_rows(data1, data2)
#  expect_error(mapbayest(ex3, data = data12))
#})
#Deprecated because we want mapbayest to fit multi ID
