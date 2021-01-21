

do_optimization <- function(arg.ofv, arg.optim, verbose, reset){
  RUN <- 1
  if(verbose) cat(paste0("\nID ", unique(arg.ofv$mrgsolve_model@args$data$ID), "..."))
  opt <- do.call(quietly(optimx), c(arg.optim, arg.ofv))$result

  while(reset && (check_etavalue(opt, arg.ofv, arg.optim)|check_convcode(OPT = opt))){
    #nam <- names(arg.optim$par)
    #new_ini <- round(runif(length(arg.optim$par), arg.optim$lower, arg.optim$upper), 6)
    #names(new_ini) <- nam
    set.seed(123+RUN)
    arg.optim$par <- new_ini(arg.ofv, arg.optim)

    warning("\nError in optimization. Reset with initial values: ", paste(arg.optim$par, collapse = ' '), call. = F, immediate. = T)
    opt <- do.call(quietly(optimx), c(arg.optim, arg.ofv))$result

    RUN <- RUN+1

  }
  opt$run <- RUN

  if(verbose) cat(" done.\n")
  return(opt)
}


check_convcode <- function(OPT) OPT$convcode!= 0
check_etavalue <- function(OPT, arg.ofv, arg.optim)isTRUE(all.equal(OPT$value, initial_ofv(arg.ofv, arg.optim)))

initial_ofv <- function(arg.ofv, arg.optim){
  do.call(compute_ofv, c(list(eta = arg.optim$par), arg.ofv))
}


new_ini <- function(arg.ofv, arg.optim){
  map2(arg.optim$lower, arg.optim$upper, runif, n = 3) %>%
    transpose() %>%
    map(diag) %>%
    map_dfr(as_tibble) %>%
    rename_with(str_replace, everything(), "V","ETA") %>%
    map(unlist) %>%
    transpose() %>%
    map(unlist) %>%
    map_dfr(function(x){
      ofv <- do.call(compute_ofv, c(list(eta = x), arg.ofv))
      c(unlist(x), OFV = ofv)
    }) %>%
    slice_min(.data$OFV, with_ties = FALSE) %>%
    select(-.data$OFV) %>%
    unlist() %>%
    round(6)
}
