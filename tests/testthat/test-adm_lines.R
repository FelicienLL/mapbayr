test_that("adm_lines.data.frame works", {
  # Single
  expect_equal(adm_lines(time = 24, amt = 100, cmt = 1),
               tibble::tibble(ID = 1L, time = 24, evid = 1L, cmt = 1L, amt = 100, mdv = 1L))
  # 1 multiple
  expect_equal(adm_lines(time = c(0, 24), amt = 100, cmt = 1),
               tibble::tibble(ID = 1L, time = c(0, 24), evid = 1L, cmt = 1L, amt = 100, mdv = 1L))
  expect_equal(adm_lines(time = 24, amt = c(100, 200), cmt = 1),
               tibble::tibble(ID = 1L, time = 24, evid = 1L, cmt = 1L, amt = c(100, 200), mdv = 1L))
  expect_equal(adm_lines(time = 24, amt = 100, cmt = c(1,2)),
               tibble::tibble(ID = 1L, time = 24, evid = 1L, cmt = c(1L, 2L), amt = 100, mdv = 1L))

  # 2 multiple
  expect_equal(adm_lines(time = c(0, 24), amt = c(100, 200), cmt = 1),
               tibble::tibble(ID = 1L, time = c(0, 24), evid = 1L, cmt = 1L, amt = c(100, 200), mdv = 1L))
  expect_equal(adm_lines(time = 24, amt = c(100, 200), cmt = c(1,2)),  # CROSS!
               tibble::tibble(ID = 1L, time = 24, evid = 1L, cmt = c(1L, 1L, 2L, 2L), amt = c(100, 200, 100, 200), mdv = 1L))
  expect_equal(adm_lines(time = c(0,24), amt = 100, cmt = c(1,2)), # CROSS!
               tibble::tibble(ID = 1L, time = c(0, 0, 24, 24), evid = 1L, cmt = c(1L, 2L, 1L, 2L), amt = 100, mdv = 1L))

  # 3 multiple
  expect_equal(adm_lines(time = c(0,24), amt = c(100, 200), cmt = c(1,2)),
               tibble::tibble(ID = 1L, time = c(0, 0, 24, 24), evid = 1L, cmt = c(1L, 2L, 1L, 2L), amt = c(100, 100, 200, 200), mdv = 1L))
  expect_equal(adm_lines(time = c(0, 24, 48), amt = c(100, 200, 300), cmt = c(1,2)),
               tibble::tibble(ID = 1L, time = c(0, 0, 24, 24, 48, 48), evid = 1L, cmt = c(1L, 2L, 1L, 2L, 1L, 2L), amt = c(100, 100, 200, 200, 300, 300), mdv = 1L))

  # Invalid number of arguments (length of vector cannot be recycled) : rarely expected, ability to cross administrations...
  expect_error(adm_lines(time = c(0,24), amt = c(100, 200, 300), rate = c(0,0,-2,0,0), cmt = 1), "Size")

  # Passing a covariate
  expect_equal(adm_lines(amt = 100, cmt = 1, BLA = 999)$BLA, 999)

})

mod0 <- mcode("mod0", "$CMT DEPOT CENT", compile = FALSE)

mod1 <- mcode("mod1", "
$PARAM DUR = 1
$CMT @annotated
DEPOT : Depot compartment () [ADM]
CENT : Central compartment ()[ADM, OBS]
$MAIN
D_CENT = DUR
", compile = FALSE)

mod2 <- mcode("mod2", "
$PARAM DUR = 1
$CMT @annotated
DEPOT : Depot compartment () [ADM]
CENT : Central compartment ()[OBS]
$MAIN
D_DEPOT = DUR
", compile = FALSE)

mod3 <- mcode("mod3", "
$CMT @annotated
DEPOT : Depot compartment () [ADM]
CENT : Central compartment ()[OBS]
", compile = FALSE)


test_that("example models are suitable for these tests", {
  #Administration compartment
  expect_null(adm_cmt(mod0))
  expect_equal(adm_cmt(mod1), c(1,2))
  expect_equal(adm_cmt(mod2), 1)
  expect_equal(adm_cmt(mod3), 1)

  #Zero order infusion to estimate => rate = -2
  expect_null(adm_0_cmt(mod0))
  expect_equal(adm_0_cmt(mod1), 2)
  expect_equal(adm_0_cmt(mod2), 1)
  expect_null(adm_0_cmt(mod3))
})

test_that("detection of default administration compartment is good",{
  expect_error(get_data(adm_lines(mod0, amt = 100))[["cmt"]], 'argument "cmt" is missing, with no default')
  expect_equal(get_data(adm_lines(mod1, amt = 100))[["cmt"]], c(1,2))
  expect_equal(get_data(adm_lines(mod2, amt = 100))[["cmt"]], 1)
  expect_equal(get_data(adm_lines(mod3, amt = 100))[["cmt"]], 1)
})

test_that("explicit cmt works well",{
  expect_equal(get_data(adm_lines(mod1, amt = 100, cmt = 1))[["cmt"]], 1)
  expect_equal(get_data(adm_lines(mod1, amt = 100, cmt = c(3, -99)))[["cmt"]], c(-99, 3)) #arrange by cmt number !
})

test_that("rate incrementation is ok",{
  expect_equal(get_data(adm_lines(mod1, amt = 100))[c("cmt","rate")], tibble::tibble(cmt = c(1,2), rate = c(0, -2)))
  expect_equal(get_data(adm_lines(mod2, amt = 100))[c("cmt","rate")], tibble::tibble(cmt = 1, rate = -2))
  expect_null(get_data(adm_lines(mod3, amt = 100))[["rate"]])
})

test_that("rate incrementation is ok with explicit cmt",{
  expect_equal(get_data(adm_lines(mod2, amt = 100, cmt = 3))[["rate"]], NULL)
  expect_equal(get_data(adm_lines(mod2, amt = 100, cmt = c(1, 3, -99)))[c("cmt","rate")], tibble::tibble(cmt = c(-99, 1, 3), rate = c(0, -2 , 0)))
})

test_that("rate incrementation is ok with explicit rate",{
  expect_equal(get_data(adm_lines(mod2, amt = 100, cmt = 3, rate = 150))[["rate"]], 150)
  expect_equal(get_data(adm_lines(mod2, amt = 100, cmt = c(1, 3, -99), rate = 150))[c("cmt","rate")], tibble::tibble(cmt = c(-99, 1, 3), rate = 150))
})

test_that("ID increment ok", {
  actual_data <- adm_lines(amt = 100, cmt = 1) %>%
    adm_lines(ID = 3, time = 1, amt = 100, cmt = 1) %>%
    adm_lines(time = 2, amt = 100, cmt = 1) %>%
    adm_lines(ID = 1, time = 3, amt = 100, cmt = 1)

  expect_equal(actual_data,
               tibble::tibble(ID = c(1L, 1L, 3L, 3L), time = c(0,3,1,2), evid = 1L, cmt = 1L, amt = 100, mdv = 1L))

})

test_that("realize addl works", {
  expect_equal(nrow(adm_lines(amt = 100, cmt = 1, addl = 9, ii = 24, realize_addl = TRUE)), 10)
})

test_that("no NA in SS, ADDL, RATE or II",{
  data1 <- mod1 %>%
    adm_lines(time = 0, amt = 10000) %>%
    adm_lines(time = 72, amt = 10000, addl = 2, ii = 24, realize_addl = TRUE) %>%
    get_data()

  expect_false(any(is.na(data1$addl)))
  expect_false(any(is.na(data1$ii)))

  data2 <- mod2 %>%
    adm_lines(time = 0, amt = 10000, ss = 1, ii = 24) %>%
    adm_lines(time = 72, amt = 10000) %>%
    get_data()

  expect_false(any(is.na(data2$ss)))
  expect_false(any(is.na(data2$ii)))

  data3 <- mod3 %>%
    adm_lines(time = 0, amt = 100, rate = 20) %>%
    adm_lines(time = 24, amt = 100) %>%
    get_data()

  expect_false(any(is.na(data3$rate)))

})
