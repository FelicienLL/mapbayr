test_that("vpc_sim() works",{
  exmodel(1, ID = 1:8) %>% vpc_sim() %>% vpc_plot()
  exmodel(301, ID = 1:8) %>% vpc_sim() %>% vpc_plot()
  exmodel(401, ID = 1:8) %>% vpc_sim() %>% vpc_plot()
})

