## est001
est001 <- mapbayest(exmodel(ID = 1:8), verbose = FALSE, progress = FALSE)
usethis::use_data(est001, overwrite = TRUE)
