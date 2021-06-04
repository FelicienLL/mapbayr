

do_optimization <- function(arg.ofv, arg.optim, verbose, reset){
  RUN <- 1
  if(verbose) cat(paste0("\nID ", unique(arg.ofv$data$ID), "..."))
  opt <- do.call(quietly(optimx), c(arg.optim, arg.ofv))$result

  while(RUN <= 50 && reset == T && check_need_reset(OPT = opt, arg.ofv = arg.ofv, arg.optim = arg.optim)){
    arg.optim$par <- new_ini2(arg.ofv, arg.optim, run = RUN)

    warning("\nError in optimization. Reset with initial values: ", paste(arg.optim$par, collapse = ' '), call. = F, immediate. = T)
    opt <- do.call(quietly(optimx), c(arg.optim, arg.ofv))$result

    RUN <- RUN + 1

  }
  opt$run <- RUN

  if(verbose) cat(" done.\n")
  return(opt)
}

check_need_reset <- function(OPT, arg.ofv, arg.optim){
  #If not all conditions TRUE = a reset is needed
  !all(
      check_convcode(OPT),
      check_finalofv(OPT, arg.ofv, arg.optim),
      check_absolute_eta(OPT = OPT, arg.ofv)
    )
}

check_convcode <- function(OPT){
  #Success condition: `convcode` variable is 0 (meaning no error returned by optim).
  OPT$convcode == 0
}

check_finalofv <- function(OPT, arg.ofv, arg.optim){
  #Success condition: final OFV is not the same than initial OFV (meaning OFV was minimized)
  ini <- initial_ofv(arg.ofv, arg.optim)
  fin <- OPT$value
  !isTRUE(all.equal(ini, fin))
}

check_absolute_eta <- function(OPT, arg.ofv){
  #Success condition: final absolute values of etas are not identical
  nam <- eta_names(arg.ofv$mrgsolve_model)
  vec <- unlist(OPT[nam])
  length(unique(abs(vec))) != 1
}

initial_ofv <- function(arg.ofv, arg.optim){
  do.call(compute_ofv, c(list(eta = arg.optim$par), arg.ofv))
}

new_ini2 <- function(arg.ofv, arg.optim, run){
  mvgauss(solve(arg.ofv$omega.inv), n = 1+n_eta(arg.ofv$mrgsolve_model)^2, seed = 1+run) %>%
    as.data.frame() %>%
    rename_with(str_replace, everything(), "V","ETA") %>%
    map(unlist) %>%
    transpose() %>%
    map(unlist) %>%
    map(~ ifelse(abs(.x) > arg.optim$upper, 0, .x)) %>%
    map_dfr(function(x){
      ofv <- do.call(compute_ofv, c(list(eta = x), arg.ofv))
      c(unlist(x), OFV = ofv)
    }) %>%
    slice_min(.data$OFV, with_ties = FALSE) %>%
    select(-.data$OFV) %>%
    unlist() %>%
    round(6)
}
