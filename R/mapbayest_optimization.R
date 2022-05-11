
#### do_optimization ####
# This is the function that realize optimization from the argument arg.ofv, arg.optim etc...

do_optimization <- function(arg.ofv, arg.optim, verbose, reset){
  try(rlang::caller_env(n = 2)$pb$tick(), silent = TRUE)

  # First the optimization is done once.
  opt <- do.call(quietly(optimx), c(arg.optim, arg.ofv))$result

  RUN <- 1
  need_new_ini <- !check_new_ini(OPT = opt, arg.ofv = arg.ofv, arg.optim = arg.optim)
  need_new_bounds <- !check_new_bounds(OPT = opt, arg.optim)

  # Secondly, if conditions for a reset are met, a new optimization is done until reset is not needed.
  while(RUN <= 50 && reset == T && (need_new_ini | need_new_bounds)){

    if(need_new_ini){
      arg.optim$par <- new_ini3(arg.ofv, arg.optim, run = RUN)
      if(verbose) message("\nDifficulty in optimization. Reset with new initial values: ", paste(arg.optim$par, collapse = ' '), call. = F, immediate. = T)
    }

    if(need_new_bounds){
      arg.optim$lower <- new_bounds(arg.ofv, arg.optim)
      arg.optim$upper <- -arg.optim$lower
      if(verbose) message("\nDifficulty in optimization. Reset with new bounds (lower displayed): ", paste(signif(arg.optim$lower), collapse = ' '), call. = F, immediate. = T)
    }

    opt <- do.call(quietly(optimx), c(arg.optim, arg.ofv))$result

    # Re-check if an additional reset is needed
    RUN <- RUN + 1
    need_new_ini <- !check_new_ini(OPT = opt, arg.ofv = arg.ofv, arg.optim = arg.optim)
    need_new_bounds <- !check_new_bounds(OPT = opt, arg.optim)

  }

  # Next chunk comes from the postprocess functions, but it did not belong there
  # Just check if OFV could be computed, otherwise return ETA = 0
  # Cannot remember the situation when it happened, and why I implemented this initially, so I cannot test it with a reprex...
  # I think it comes from a time when there was no "reset" routine, but it is probably irrelevant now because I never see this message in performance tests.
  # Still keeping it anyway...

  if(!is.null(opt$fevals)){
    if(is.nan(opt$fevals)) {
      opt[eta_names(arg.ofv$qmod)] <- 0
      warning("\nCannot compute objective function value ; typical value (ETA = 0) returned")
    }
    if(is.na(opt$fevals)) {
      opt[eta_names(arg.ofv$qmod)] <- 0
      warning("\nCannot minimize objective function value ; typical value (ETA = 0) returned")
    }
  }

  opt$run <- RUN
  return(opt)
}




#### check results of optimization ####
# All the functions return TRUE if there is no problem
# 1.1 -  One function to test all conditions for new initial values.

check_new_ini <- function(OPT, arg.ofv, arg.optim){
  #If not all conditions TRUE, will return FALSE
  all(
    check_convcode(OPT),
    check_finalofv(OPT, arg.ofv, arg.optim),
    check_absolute_eta(OPT = OPT)
  )
}

# 1.2 -  Unit functions that test a particular condition for new initial values
check_convcode <- function(OPT){
  #Success condition: `convcode` variable is 0 (meaning no error returned by optim).
  OPT$convcode == 0
}

check_finalofv <- function(OPT, arg.ofv, arg.optim){
  #Success condition: final OFV is not the same than initial OFV (meaning OFV was minimized)
  ini <- do_compute_ofv(eta = arg.optim$par, argofv = arg.ofv)
  fin <- OPT$value
  !isTRUE(all.equal(ini, fin))
}

check_absolute_eta <- function(OPT){
  #Success condition: final absolute values of etas are not identical
  vec <- eta_from_opt(OPT)
  if(length(vec)==1){
    return(TRUE)
  } else {
    return(length(unique(abs(vec))) != 1 )
  }
}

# 2.1 One function to test condition for new bounds.
check_new_bounds <- function(OPT, arg.optim){
  #Success condition : no eta equal to a bound
  if(arg.optim$method != "L-BFGS-B") return(TRUE)
  vec <- eta_from_opt(OPT)
  if(any(is.na(vec))) return(TRUE)
  !any(vec == arg.optim$lower,
       vec == arg.optim$upper)
}


#### Generate new arguments for the reset  ####
# 1 Returns a vector of ETA to start the new estimation from:

new_ini2 <- function(arg.ofv, arg.optim, run){
  mvgauss(solve(arg.ofv$omega_inv), n = 1+n_eta(arg.ofv$qmod)^2, seed = 1+run) %>% #Sample 1+(Neta x Neta) vectors from prior MVN distribution
    as.data.frame() %>%
    rename_with(str_replace, everything(), "V","ETA") %>%
    map(unlist) %>%
    transpose() %>%
    map(unlist) %>%
    map(~ ifelse(abs(.x) > arg.optim$upper, 0, .x)) %>%   # If out of bounds value of eta, set it to zero.
    map_dfr(function(x){                                  # Compute OFV for every vector value
      ofv <- do.call(compute_ofv, c(list(eta = x), arg.ofv))
      c(unlist(x), OFV = ofv)
    }) %>%
    slice_min(.data$OFV, with_ties = FALSE) %>%           # Keep the lowest one
    select(-.data$OFV) %>%
    unlist() %>%
    round(6)
}

# 2 Returns a vector of lower bounds

new_bounds <- function(arg.ofv, arg.optim){
  vec_SE <- sqrt(diag(solve(arg.ofv$omega_inv)))
  P <- map2_dbl(.y = vec_SE, .x = arg.optim$lower, .f = stats::pnorm, mean = 0)
  P <- P[1]
  new_P <- P/10
  map_dbl(vec_SE, stats::qnorm, p = new_P, mean = 0)
}

new_ini3 <- function(arg.ofv, arg.optim, run){
  neta <- n_eta(arg.ofv$qmod)
  nsim <- 1 + neta ^ 2

  # Sample eta from prior distribution
  simmat <- mvgauss(solve(arg.ofv$omega_inv), n = nsim, seed = 1+run)

  # Set Out-of-bound etas to 0
  bound <- arg.optim$upper
  for(i in seq_len(neta)){
    vals <- simmat[,i]
    simmat[,i] <- ifelse(abs(vals) > bound[i], 0, vals)
  }

  # Compute OFV for each vector of eta
  colnames(simmat) <- paste0("ETA", seq_len(neta))
  list_etas <- apply(simmat, 1, as.list)
  ofvs <- sapply(list_etas, do_compute_ofv, argofv = arg.ofv)

  # Return etas with lowest OFV
  round(simmat[which.min(ofvs),], 6)
}
