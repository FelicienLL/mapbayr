test_that("replicate_data() works", {
  expect_equal(
    replicate_data(data.frame(ID = c(1,1,2,3,3,3), time = c(0,24,0,0,24,48)), n = 2),
    matrix(c(
      c(1,1,2,3,3,3,4,4,5,6,6,6),
      c(0,24,0,0,24,48,0,24,0,0,24,48)
    ), ncol = 2, dimnames = list(NULL, c("ID","time"))
    )
  )
})

test_that("vpc_sim() works",{
  exmodel(1, ID = 1:8) %>% vpc_sim() %>% vpc_plot()
  exmodel(301, ID = 1:8) %>% vpc_sim() %>% vpc_plot()
  exmodel(401, ID = 1:8) %>% vpc_sim() %>% vpc_plot()
})

