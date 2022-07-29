mod <- mcode("mod",
             "
             $CMT @annotated
             GUT: gut
             CENT: central [OBS]
             ", compile = FALSE)
data <- exdata()

test_that("Compartment in data is ok with cmt defined in model", {
  faildata_mdv0 <- rbind(data, c(ID = 1, time = 30, evid = 0, amt = 0, cmt = 3, ii = 0, addl = 0, mdv = 0, DV = 15))
  expect_error(check_mapbayr_modeldata(mod, faildata_mdv0),
               "One or multiple line\\(s\\) with cmt = 3 observed in data, but only 2 compartments defined in model\\.")

  faildata_mdv1 <- rbind(data, c(ID = 1, time = 30, evid = 0, amt = 0, cmt = 3, ii = 0, addl = 0, mdv = 1, DV = 15))
  expect_error(check_mapbayr_modeldata(mod, faildata_mdv1),
               "One or multiple line\\(s\\) with cmt = 3 observed in data, but only 2 compartments defined in model\\.")
})

test_that("Observation compartment in data are those defined with [OBS] in model, if any", {
  dat <- data.frame(ID = 1, mdv = 0, cmt = c(1,2))
  expect_error(check_mapbayr_modeldata(mod, dat),
               ".*One or more compartment with observation \\(mdv=0\\) in data don\\'t match those defined with \\[OBS\\] in \\$CMT\\.")
})
