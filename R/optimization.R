

do_optimization <- function(arg.ofv, arg.optim, verbose){
  if(verbose) cat(paste0("\nID ", unique(arg.ofv$mrgsolve_model@args$data$ID), "..."))
  opt <- do.call(quietly(optimx), c(arg.optim, arg.ofv))$result

  while(opt$value == initial_ofv(arg.ofv, arg.optim)){
    nam <- names(arg.optim$par)
    new_ini <- round(runif(length(arg.optim$par), -1, 1), 6)
    names(new_ini) <- nam
    arg.optim$par <- new_ini

   warning("\nNo change in OFV after optimization. Rerun with initial values: ", paste(new_ini, collapse = ' '), call. = F, immediate. = T)
    opt <- do.call(quietly(optimx), c(arg.optim, arg.ofv))$result
  }
  if(verbose) cat(" done.\n")
  return(opt)
}

initial_ofv <- function(arg.ofv, arg.optim){
  do.call(compute_ofv, c(list(eta = arg.optim$par), arg.ofv))
}
