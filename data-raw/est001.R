## est001
dat001 <- read.csv(system.file("nm001/data_to_fit001.csv", package = "mapbayr"), na = ".")
mod001 <- mrgsolve::mread(system.file("nm001/mrg_001.cpp", package = "mapbayr"))
est001 <- mapbayest(mod001, dat001, verbose = FALSE)
usethis::use_data(est001, overwrite = TRUE)
