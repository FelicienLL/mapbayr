
#### do_optimization ####
# This is the function that realize optimization from the argument arg.ofv, arg.optim etc...

keep_argofv <- function(x){
  x[c("qmod",  "sigma", "log_transformation", "omega_inv", "all_cmt", "idvaliddata", "idDV", "idcmt")]
}

do_optimization <- function(..., verbose = TRUE, reset = 50){
  try(rlang::caller_env(n = 2)$pb$tick(), silent = TRUE)
  args <- list(...)

  optimizer <- switch(args$method,
                      "L-BFGS-B" = quietly(stats::optim),
                      "newuoa" = quietly(minqa::newuoa)
  )

  nreset <- 0

  while(nreset == 0 || (nreset <= reset && (need_new_ini | need_new_bounds))){
    if(nreset != 0 && need_new_ini){
      args$par <- new_ini3(arg.ofv = keep_argofv(args), upper = args$upper, nreset = nreset, select_eta = args$select_eta)
      if(verbose){
        if(nreset == 1) cat("\n")
        message("Reset with new initial values: ", paste(args$par, collapse = ' '))
      }
    }

    if(nreset != 0 && need_new_bounds){
      args$lower <- new_bounds(omega_inv = args$omega_inv, lower = args$lower)
      args$upper <- -args$lower
      if(verbose) {
        if(nreset == 1) cat("\n")
        message("Reset with new bounds (lower displayed): ", paste(signif(args$lower), collapse = ' '))
      }
    }

    opt <- do.call(optimizer, args)$result
    opt$nreset <- nreset
    nreset <- nreset + 1
    need_new_ini <- !check_new_ini(OPT = opt, arg.ofv = keep_argofv(args), par = args$par)
    need_new_bounds <- !check_new_bounds(OPT = opt, args$method, args$lower, args$upper)
  }

  # Next chunk comes from the postprocess functions, but it did not belong there
  # Just check if OFV could be computed, otherwise return ETA = 0
  # Cannot remember the situation when it happened, and why I implemented this initially, so I cannot test it with a reprex...
  # I think it comes from a time when there was no "reset" routine, but it is probably irrelevant now because I never see this message in performance tests.
  # Still keeping it anyway...

  if(!is.null(opt$fevals)){
    if(is.nan(opt$fevals)) {
      opt[names(args$par)] <- 0
      warning("\nCannot compute objective function value ; typical value (ETA = 0) returned")
    }
    if(is.na(opt$fevals)) {
      opt[names(args$par)] <- 0
      warning("\nCannot minimize objective function value ; typical value (ETA = 0) returned")
    }
  }

  if(inherits(opt, "minqa")){
    opt$par <- rename_as_eta(opt$par)
  }

  return(opt)
}




#### check results of optimization ####
# All the functions return TRUE if there is no problem
# 1.1 -  One function to test all conditions for new initial values.

check_new_ini <- function(OPT, par, arg.ofv){
  #If not all conditions TRUE, will return FALSE
  all(
    check_convergence(OPT),
    check_finalofv(OPT, par, arg.ofv),
    check_absolute_eta(OPT = OPT)
  )
}

# 1.2 -  Unit functions that test a particular condition for new initial values
check_convergence <- function(OPT){
  #Success condition: `convergence` variable is 0 (meaning no error returned by optim).
  if(inherits(OPT, "minqa")){
    OPT$ierr == 0
  } else {
    OPT$convergence == 0
  }
}

check_finalofv <- function(OPT, par, arg.ofv){
  #Success condition: final OFV is not the same than initial OFV (meaning OFV was minimized)
  ini <- do_compute_ofv(eta = par, argofv = arg.ofv)
  if(inherits(OPT, "minqa")){
    fin <- OPT$fval
  } else {
    fin <- OPT$value
  }
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
check_new_bounds <- function(OPT, method, lower, upper){
  #Success condition : no eta equal to a bound
  if(method != "L-BFGS-B") return(TRUE)
  vec <- eta_from_opt(OPT)
  if(any(is.na(vec))) return(TRUE)
  !any(vec == lower,
       vec == upper)
}


#### Generate new arguments for the reset  ####
# 1 Returns a vector of ETA to start the new estimation from:

new_ini2 <- function(arg.ofv, arg.optim, run){
  mvgauss(solve(arg.ofv$omega_inv), n = 1+eta_length(arg.ofv$qmod)^2, seed = 1+run) %>% #Sample 1+(Neta x Neta) vectors from prior MVN distribution
    as.data.frame() %>%
    rename_with(str_replace, everything(), "V","ETA") %>%
    map(unlist) %>%
    transpose() %>%
    map(unlist) %>%
    map(~ ifelse(abs(.x) > arg.optim$upper, 0, .x)) %>%   # If out of bounds value of eta, set it to zero.
    purrr::map_dfr(function(x){                                  # Compute OFV for every vector value
      ofv <- do.call(compute_ofv, c(list(eta = x), arg.ofv))
      c(unlist(x), OFV = ofv)
    }) %>%
    dplyr::slice_min(.data$OFV, with_ties = FALSE) %>%           # Keep the lowest one
    select(-.data$OFV) %>%
    unlist() %>%
    round(6)
}

# 2 Returns a vector of lower bounds
new_bounds <- function(omega_inv, lower){
  vec_SE <- sqrt(diag(solve(omega_inv)))
  P <- map2_dbl(.y = vec_SE, .x = lower, .f = stats::pnorm, mean = 0)
  P <- P[1]
  new_P <- P/10
  sapply(vec_SE, stats::qnorm, p = new_P, mean = 0)
}

new_ini3 <- function(arg.ofv, upper, nreset, select_eta){
  nsim <- 1 + length(select_eta) ^ 2

  # Sample eta from prior distribution
  simmat <- mvgauss(solve(arg.ofv$omega_inv), n = nsim, seed = 1+nreset)
  colnames(simmat) <- make_eta_names(select_eta)

  # Set Out-of-bound etas to 0
  bound <- upper
  if(!(length(bound) == 1 && !is.null(bound))){ #prevent fail if bound = NULL with newuoa
    for(i in seq_len(ncol(simmat))){
      vals <- simmat[,i]
      simmat[,i] <- ifelse(abs(vals) > bound[i], 0, vals)
    }
  }

  # Compute OFV for each vector of eta
  list_etas <- apply(simmat, 1, as.list)
  ofvs <- sapply(list_etas, do_compute_ofv, argofv = arg.ofv)

  # Return etas with lowest OFV
  round(simmat[which.min(ofvs),], 6)
}
