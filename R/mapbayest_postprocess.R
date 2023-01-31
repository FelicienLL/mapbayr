post_eta <- function(x){
  do.call(rbind, sapply(x, eta_from_opt, simplify = F))
}

dataeta <- function(data, eta){
  #data a data set
  #eta a matrix of eta (no ID column)
  left_join(x = data, y = mutate(as.data.frame(eta), ID = as.double(rownames(eta))), by = "ID")
}


post_mapbay_tab <- function(x, data, etamat){
  # PRED
  pred <- mrgsim_df(zero_re(x), data, Req = "DV")[["DV"]]

  # IPRED and POST HOC parameters
  dataposthoc <- dataeta(data = data, eta = etamat)
  capturednames <- outvars(x)$capture
  posthocsims <- mrgsim_df(zero_re(x), dataposthoc, Req = capturednames) %>%
    rename(IPRED = "DV") %>%
    select(-all_of(c("ID", "time")))

  mapbay_tab <- cbind(dataposthoc, PRED = pred, posthocsims)

  # REMAINING COVARIATES (not in data and not captured)
  all_covs <- mbr_cov_names(x)
  captured_covs <- all_covs[all_covs %in% capturednames]
  data_covs <- all_covs[all_covs %in% names(data)]
  missing_covs <- all_covs[!all_covs %in% c(captured_covs, data_covs)]

  if(length(missing_covs) > 0){
    missing_cov_vals <- param(x)[[missing_covs]]
    names(missing_cov_vals) <- missing_covs
    mapbay_tab <- cbind(mapbay_tab, as.data.frame(as.list(missing_cov_vals)))
  }

  # RELOCATE NAMES
  namesdata <- names(data)
  namesdata <- namesdata[!namesdata %in% c("DV", all_covs)]
  mapbay_tab %>%
    relocate(any_of(c(namesdata, "DV", "IPRED", "PRED", capturednames, all_covs)), starts_with("ETA"))
}

safe_solve <- safely(solve, otherwise = matrix(NA_real_))

post_covariance <- function(arg.ofv.id, final_eta, x, hessian, arg.optim, arg.ofv.fix){
  accepted_args <- names(formals(hessian))

  if(all(c("par", "fn") %in% accepted_args)){

    fp <- function(p){ #obj fun value as function of param
      arg <- c(arg.ofv.fix, arg.ofv.id)
      eta <- p
      names(eta) <- make_eta_names(x = arg.optim$select_eta)
      arg$eta <- eta
      do.call(compute_ofv, arg)
    }

    all_args_to_pass <- list(par = final_eta[arg.optim$select_eta],
                             fn = fp,
                             control = arg.optim$control)
    actual_args <- all_args_to_pass[intersect(names(all_args_to_pass), accepted_args)]
    hess <- do.call(hessian, args = actual_args)
    covariance_selected <- unname(2 * safe_solve(hess)$result)

    # fill with 0 for non-selected ETAs
    covariance <- matrix(0, ncol = eta_length(x), nrow = eta_length(x))
    covariance[arg.optim$select_eta, arg.optim$select_eta] <- covariance_selected
    covariance

  } else {
    covariance <- matrix(NA_real_)
  }
}

generate_information <- function(times){
  version <- c(
    mapbayr = as.character(utils::packageVersion("mapbayr")),
    mrgsolve = as.character(utils::packageVersion("mrgsolve")),
    stats = as.character(utils::packageVersion("stats")),
    minqa = tryCatch(utils::packageVersion("minqa"), silent = TRUE, error = function(x)NA)
  )
  times[4] <- Sys.time()
  list(
    start = times[1],
    end = times[4],
    duration = as.double.difftime(times[4]-times[1], units = "secs"),
    details = c(
      preprocessing = as.double.difftime(times[2]-times[1], units = "secs"),
      optimization = as.double.difftime(times[3]-times[2], units = "secs"),
      postprocessing = as.double.difftime(times[4]-times[3], units = "secs")
    ),
    version = version
  )
}
