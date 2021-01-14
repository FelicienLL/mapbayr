

do_optimization <- function(arg.ofv, arg.optim, verbose, reset){
  if(verbose) cat(paste0("\nID ", unique(arg.ofv$mrgsolve_model@args$data$ID), "..."))
  opt <- do.call(quietly(optimx), c(arg.optim, arg.ofv))$result

  check_convcode <- function(OPT) OPT$convcode!= 0
  check_etavalue <- function(OPT)isTRUE(all.equal(OPT$value, initial_ofv(arg.ofv, arg.optim)))

  while(reset && (check_etavalue(opt)|check_convcode(opt))){
    nam <- names(arg.optim$par)
    new_ini <- round(runif(length(arg.optim$par), arg.optim$lower, arg.optim$upper), 6)
    names(new_ini) <- nam
    arg.optim$par <- new_ini

    warning("\nError in optimization. Reset with initial values: ", paste(new_ini, collapse = ' '), call. = F, immediate. = T)
    opt <- do.call(quietly(optimx), c(arg.optim, arg.ofv))$result
  }
  if(verbose) cat(" done.\n")
  return(opt)
}

initial_ofv <- function(arg.ofv, arg.optim){
  do.call(compute_ofv, c(list(eta = arg.optim$par), arg.ofv))
}
