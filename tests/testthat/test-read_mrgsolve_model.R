test_that("_cmt functions work", {

  #Models
  #Not annotated
  mod0 <- mcode("mod0", "$CMT GUT CENT", compile = FALSE)
  #Annotated but empty
  mod0bis <- mcode("mod0", "
                $CMT @annotated
                GUT : Gut ()
                CENT: Central ()", compile = FALSE)
  #Annotated, one of each
  mod1 <- mcode("mod1",
                "$CMT @annotated
                GUT : Gut () [ADM]
                CENT: Central () [OBS]
                PERIPH: Periph ()", compile = FALSE)
  #Annotated, two of each
  mod2 <- mcode("mod2",
                "$CMT @annotated
                GUT : Gut () [ADM]
                CENT: Central () [ADM,OBS]
                PERIPH: Periph ()
                MET : Metabolite () [OBS]", compile = FALSE)

  #Data
  dat0 <- data.frame(ID = 1, mdv = 1, cmt = 1)
  dat1 <- data.frame(ID = 1, mdv = 0, cmt = 1)
  dat2 <- data.frame(ID = 1, mdv = 0, cmt = 2)
  dat12<- data.frame(ID = 1, mdv = 0, cmt = c(1,2))

  expect_equal(adm_cmt(mod0), NULL)
  expect_equal(adm_cmt(mod0bis), NULL)
  expect_equal(adm_cmt(mod1), 1)
  expect_equal(adm_cmt(mod2), c(1,2))

  expect_equal(obs_cmt(mod0), NULL)
  expect_equal(obs_cmt(mod0bis), NULL)
  expect_equal(obs_cmt(mod1), 2)
  expect_equal(obs_cmt(mod2), c(2,4))

  expect_equal(obs_cmt_data(dat0), NULL)
  expect_equal(obs_cmt_data(dat1), 1)
  expect_equal(obs_cmt_data(dat2), 2)
  expect_equal(obs_cmt_data(dat12), c(1,2))

  expect_equal(fit_cmt(mod0, dat0), NULL)
  expect_equal(fit_cmt(mod0, dat1), 1)
  expect_equal(fit_cmt(mod0, dat2), 2)
  expect_equal(fit_cmt(mod0, dat12), c(1,2))
  expect_equal(fit_cmt(mod0bis, dat0), NULL)
  expect_equal(fit_cmt(mod0bis, dat1), 1)
  expect_equal(fit_cmt(mod0bis, dat2), 2)
  expect_equal(fit_cmt(mod0bis, dat12), c(1,2))
  expect_equal(fit_cmt(mod1, dat0), 2)
  expect_equal(fit_cmt(mod1, dat1), 2)
  expect_equal(fit_cmt(mod1, dat2), 2)
  expect_equal(fit_cmt(mod1, dat12),2)
  expect_equal(fit_cmt(mod2, dat0), c(2,4))
  expect_equal(fit_cmt(mod2, dat1), c(2,4))
  expect_equal(fit_cmt(mod2, dat2), c(2,4))
  expect_equal(fit_cmt(mod2, dat12), c(2,4))
})

test_that("adm_0_cmt works", {
  #Models
  #No $MAIN
  mod0 <- mcode("mod0",
                "$CMT GUT CENT
                ", compile = FALSE)
  #$MAIN but empty
  mod0bis <- mcode("mod0bis",
                "$CMT GUT CENT
                $MAIN
                ", compile = FALSE)
  #$MAIN but no D_
  mod0ter <- mcode("mod0ter",
                "$CMT GUT CENT
                $MAIN
                double CL = 0.1 ;
                ", compile = FALSE)
  #One D_
  mod1 <- mcode("mod1",
                "$CMT GUT CENT
                $MAIN
                double CL = 0.1 ;
                D_GUT = 1.1;
                ", compile = FALSE)
  #Two D_
  mod2 <- mcode("mod2",
                "$CMT GUT CENT
                $MAIN
                double CL = 0.1 ;
                D_GUT = 1.1;
                D_CENT = 2.2;
                ", compile = FALSE)

  expect_equal(adm_0_cmt(mod0), NULL)
  expect_equal(adm_0_cmt(mod0bis), NULL)
  expect_equal(adm_0_cmt(mod0ter), NULL)
  expect_equal(adm_0_cmt(mod1), 1)
  expect_equal(adm_0_cmt(mod2), c(1,2))
})

test_that("log_transformation works", {
  expect_true(log_transformation(mcode("mod","$TABLE double DV = exp(EPS(2)) ; ", compile = FALSE)))
  expect_true(log_transformation(mcode("mod","$TABLE double DV = exp ( EPS(2)) ; ", compile = FALSE)))
  expect_false(log_transformation(mcode("mod","$TABLE double DV = 1 + EPS(2) ; ", compile = FALSE)))
})

test_that("log_transformation works with sigma labels", {
  sigma_block <- "
                $SIGMA @annotated
                PROP: 0.1 : Proportional
                ADD : 0.2 : Additive
                $SIGMA @annotated
                PROP2 : .3: Proportional 2
                $SIGMA
                0.4
  "
  expect_true(log_transformation(mcode("mod", paste(sigma_block, "$TABLE double DV = exp(EPS(1)) ;"), compile = FALSE)))
  expect_true(log_transformation(mcode("mod", paste(sigma_block, "$TABLE double DV = exp(PROP) ;"), compile = FALSE)))
  expect_true(log_transformation(mcode("mod", paste(sigma_block, "$TABLE double DV = exp(ADD) ;"), compile = FALSE)))
  expect_true(log_transformation(mcode("mod", paste(sigma_block, "$TABLE double DV = exp(PROP2) ;"), compile = FALSE)))
  expect_true(log_transformation(mcode("mod", paste(sigma_block, "$TABLE double DV = exp(EPS(4)) ;"), compile = FALSE)))
})

test_that("eta_descr works", {
  mod87 <- mcode("mod87", "$PARAM ETA1 = 0, ETA2 = 0
$PARAM @annotated @covariate
BW : 50 : Body weight (kg)", compile = FALSE)

  expect_equal(eta_descr(mod87), c("ETA1", "ETA2"))

  mod87bis <- mcode("mod87bis",
                    "$PARAM @annotated
              ETA1 : 0 : Clearance
              ETA2 : 0 :
$PARAM @annotated @covariate
BW : 50 : Body weight (kg)", compile = FALSE)

  expect_equal(eta_descr(mod87bis), c("Clearance", "ETA2"))

  mod87ter <- mcode("mod87bis",
                    "$PARAM ETA1 = 0, ETA2 = 0", compile = FALSE)

  expect_equal(eta_descr(mod87ter), c("ETA1", "ETA2"))

})
