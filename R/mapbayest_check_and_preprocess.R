#' Check if model is valid for 'mapbayr'
#'
#' @description
#' Checks that the model respects points related exclusively to 'mapbayr'. Useful at the time you wish to convert a "regular" 'mrgsolve' model you used for simulation into a model to perform MAP-Bayesian estimation.
#' Note that some elements cannot be checked:
#' - In `$MAIN` block, make sure that you added `ETA1, ETA2...` in the code. For instance: `double CL = TVCL * exp(ETA(1) + ETA1) ;`.
#' - In `$OMEGA` block, make sure the order of the (diagonal) values is the same as for ETAs in `$PARAM`. For instance, if `ETA1` corresponds to clearance, the first value in `$OMEGA` must be the variance of clearance.
#' - In `$SIGMA` block, make sure the order is respected: proportional error first, and additive error secondly.
#'
#' @param x model file
#' @param check_compile check if model is compiled
#'
#' @return `TRUE` (invisibly) if checks are passed, errors otherwise.
#' @export
#'
#' @examples
#' library(mapbayr)
#' library(mrgsolve)
#' \dontrun{check_mapbayr_model(house())}
check_mapbayr_model <- function(x, check_compile = TRUE){
  if(!is.mrgmod(x)){
    stop("the first argument must be a model object", call. = F)
  } else {
    # Structure

    if(!is.list(x@param@data)){
      stop("mod@param@data is not a list")
    }

    if(check_compile){
      if(!x@shlib$compiled){
        stop('model object is not compiled')
      }
      # Check if shared object is loaded. If not it errors.
      mrgsolve::loadso(x)
    }

    # $PARAM
    eta_names_x <- eta_names(x)
    neta <- length(eta_names_x)
    if(neta == 0) {
      stop('$PARAM. Cannot find parameters named "ETA1", "ETA2", etc... \nDid you forget to add these parameters in $PARAM?', call. = FALSE)
    } else {
      expected_eta_names <- paste0("ETA", seq_along(eta_names_x))
      if(any(eta_names_x != expected_eta_names)){
        stop(paste0("$PARAM. ", neta, " ETA parameter(s) found, but not named ", paste(expected_eta_names, collapse = ", "), ". "), call. = FALSE)
      }
      if(!all(x[eta_names_x]==0)){
        stop(paste0("$PARAM. The value of one or multiple ETA parameter(s) is not 0."), call. = FALSE)
      }
    }

    # $OMEGA
    odiag_x <- odiag(x)
    nomega <- length(odiag_x)
    if(nomega != neta) {
      stop(paste0("$OMEGA. The OMEGA matrix diagonal has length ", nomega, ", but ", neta, " ETA parameters are defined in $PARAM."), call. = FALSE)
    }
    if(any(odiag_x == 0)){
      stop("$OMEGA. The value of one or multiple OMEGA value is equal to 0. Cannot accept value in OMEGA equal to zero.", call. = FALSE)
    }

    # $SIGMA
    sdiag_x <- diag(smat(x, make = T))
    if(all(sdiag_x == 0)){
      stop("$SIGMA. All the values in $SIGMA are equal to zero, which is not allowed.", call. = FALSE)
    }
    nsig <- length(sdiag_x)
    if(nsig %% 2 != 0){
      stop(paste0("$SIGMA. The SIGMA matrix diagonal has length ", nsig, ". A pair number is expected."), call. = FALSE)
    }

    obs_cmt_x <- obs_cmt(x)
    if(is.null(obs_cmt_x)){
      if(nsig != 2){
        stop("$SIGMA. More than 2 values defined in $SIGMA, while [OBS] was not defined in $CMT.", call. = FALSE)
      }
    } else {
      ncmt <- length(obs_cmt_x)
      if(nsig != ncmt * 2){
        stop(paste0("$SIGMA. ", nsig, " values defined in $SIGMA, but ", ncmt * 2, " were expected. Define one pair of sigma values (prop + add errors) per [OBS] compartment(s) defined in $CMT."), call. = FALSE)
      }
    }

    if(log_transformation(x)){
      if(any(which(sdiag_x==0)%%2 == 0)){
        stop("$SIGMA. Values in position 2,4... (i.e. additive) cannot be equal to 0 if residual error is defined as exponential in $TABLE", call. = FALSE)
      }
      if(any(which(sdiag_x!=0)%%2 != 0)){
        stop("$SIGMA. Values in position 1,3...(i.e. proportional) must be equal to 0 if residual error is defined as exponential in $TABLE", call. = FALSE)
      }
    }

    # $CAPTURE
    if("PRED" %in% x@capL){
      stop("$CAPTURE. PRED found in $CAPTURE. Do not set PRED in $CAPTURE.", call. = FALSE)
    }
    if("IPRED" %in% x@capL){
      stop("$CAPTURE. IPRED found in $CAPTURE. Do not set IPRED in $CAPTURE.", call. = FALSE)
    }
    if(any(eta_names_x %in% x@capL)){
      stop("$CAPTURE. ETAn found in $CAPTURE. Do not set ETA1, ETA2 etc... in $CAPTURE.", call. = FALSE)
    }
    if(!"DV" %in% x@capL){
      stop("$CAPTURE. Cannot find DV in captured items. DV must be captured", call. = FALSE)
    }
    if(any(!(c("PAR", "MET") %in% x@capL)) & nsig > 2){
      stop("$CAPTURE. Cannot find PAR and MET in captured items. They must be captured if multiple types of DV are fitted (more than one pair of sigma provided in $SIGMA)", call. = FALSE)
    }

  }
  return(invisible(TRUE))
}

check_mapbayr_data <- function(data){
  # Is there any data?
  if(is.null(data)) stop("No data provided", call. = F)

  # Remove .datehour, if any
  data[[".datehour"]] <- NULL

  # Are all column numerics
  non_num <- names(data)[!sapply(data, is.numeric)]
  if(length(non_num)) stop(paste("Non-numeric column found:", paste(non_num, collapse = " ")), call. = F)

  # Are required items present?
  data <- data %>%
    rename_with(tolower, any_of(c("TIME", "AMT", "MDV", "CMT", "EVID", "II", "ADDL", "SS", "RATE")))

  required_nmtran_item <- c("ID", "time", "evid", "cmt", "amt", "DV")
  miss_item <- required_nmtran_item[!(required_nmtran_item %in% names(data))]
  if(length(miss_item)) stop(paste("Missing column:", paste(miss_item, collapse = " ")), call. = F)
  if(is.null(data[["mdv"]])){
    data[["mdv"]] <- ifelse(data[["evid"]] %in% c(1,2,4), 1, 0)
  }
  if(is.null(data[["mdv"]]))  stop('mdv column is missing', call. = F) #Cannot happen obviously... but who knows

  # Are MDV/EVID requirements respected?

  if(nrow(filter(data, .data$mdv == 0 & .data$evid == 2)) > 0) stop("Lines with evid = 2 & mdv = 0 are not allowed", call. = F)
  if(nrow(filter(data, .data$mdv == 0 & .data$evid != 0)) > 0) stop("Lines with mdv = 0 must have evid = 0.", call. = F)
  if(nrow(filter(data, .data$time == 0, .data$mdv == 0)) > 0)  stop("Observation line (mdv = 0) not accepted at time = 0", call. = F)
  if(any(data$mdv==0 & is.na(data$DV))) stop("DV cannot be missing (NA) on an observation line (mdv = 0)", call. = F)
  return(data)
}



check_mapbayr_modeldata <- function(x, data){
  # --- Checks full data vs model

  varinmodel <- c(names(x@param), as.list(x)$cpp_variables$var)
  varinmodel <- varinmodel[!varinmodel %in% c("DV", mbr_cov_names(x))]
  varindata <- names(data)
  commonvar <- varindata[varindata %in% varinmodel]

  if(length(commonvar) > 0) {
    stop("Variables found both in the model (`$PARAM`) and in the data: ",
         paste(commonvar, collapse = ", "),
         ".\nIf these are covariates, please declare them with the `@annotated @covariates` tags in `$PARAM`.\n",
         "Otherwise, remove them from the data.",
         call. = FALSE)
  }

  cmt_in_data <- unique(data$cmt)
  max_cmt_mod <- length(x$cmt)
  invalid_cmt_in_data <- cmt_in_data[cmt_in_data > max_cmt_mod]

  if(any(as.logical(invalid_cmt_in_data))) stop("One or multiple line(s) with cmt = ", paste(invalid_cmt_in_data, collapse = " "), " observed in data, but only ", max_cmt_mod, " compartments defined in model.", call. = FALSE)

  cmt_data <- obs_cmt_data(data)
  cmt_model <- obs_cmt(x)
  if(is.null(cmt_model)){
    if(length(cmt_data)!=1) stop(paste0("ID =", data$ID[1], "; CMT =", paste(cmt_data, collapse = " "), "\nMore than one `observation compartment` found in data. Consider editing model code with [OBS] in $CMT."), call. = F)
    if(any(!(cmt_data %in% x@Icmt))) stop(paste0("ID =", data$ID[1], "; CMT =", cmt_data, "\n Compartment number with observation in dataset does not exist in model."))
  } else {
    if(any(!cmt_data %in% cmt_model)) stop(paste0("ID =", data$ID[1], "; CMT =", paste(cmt_data, collapse = " "), "\n One or more compartment with observation (mdv=0) in data don't match those defined with [OBS] in $CMT."), call. = F)
  }


}


split_mapbayr_data <- function(data){
  # --- Data split by ID
  split(data, ~factor(ID, levels = unique(data$ID)))
}


#' Pre-process: arguments for optimization function
#'
#' @inheritParams mapbayest
#'
#' @return a list of named arguments passed to optimizer (i.e. arg.optim)
#' @export
preprocess.optim <- function(x, method = c("L-BFGS-B", "newuoa"), select_eta = NULL, control = list(), force_initial_eta = NULL, quantile_bound = 0.001){
  #Checks argument

  #method
  method <- method[1]
  okmethod <- c("L-BFGS-B", "newuoa")
  if(!method %in% okmethod) stop(paste("Accepted methods:", paste(okmethod, collapse = ", "), '.'))
  netas <- eta_length(x)
  if(is.null(select_eta)){
    select_eta <- seq_len(netas)
  }

  if(method == "newuoa"){
    if(!requireNamespace("minqa", quietly = TRUE)) {
      stop(
        "Package \"minqa\" must be installed to use method = \"newuoa\" ",
        call. = FALSE
      )
    }

    # Call minqa::newuoa(par, fn, control = list(), ...)

    # par
    initial_eta <- force_initial_eta
    if(is.null(initial_eta)){
      initial_eta <- eta(n = netas, val = 0.01)[select_eta]
    }

    # fn = compute_ofv

    # control = list(npt, rhobeg, rhoend, iprint, maxfun)
    if(is.null(control$iprint)){
      control$iprint <- 0
    }

    arg <- list(
      par = initial_eta,
      fn = compute_ofv,
      control = control,
      method = method, # I still keep it for the wrappers around newuoa
      select_eta = select_eta
    )
  }

  if(method == "L-BFGS-B"){

    # Call stats::optim(par, fn, gr = NULL, ...,
    #                   method = c("Nelder-Mead", "BFGS", "CG", "L-BFGS-B", "SANN",
    #                              "Brent"),
    #                   lower = -Inf, upper = Inf,
    #                   control = list(), hessian = FALSE)

    # par
    initial_eta <- force_initial_eta
    if(is.null(initial_eta)){
      initial_eta <- eta(n = netas)[select_eta]
    }

    # fn = compute_ofv, gr = NULL, hessian = FALSE

    # method = "L-BFGS-B"

    # lower, upper
    bound <- get_quantile(x, .p = quantile_bound)[select_eta]

    # control = list(trace,
    #                fnscale,
    #                parscale, ndeps,
    #                maxit,
    #                abstol, reltol, alpha, beta, gamma,
    #                REPORT, warn.1d.NelderMead, type,
    #                lmm,   # <-- L-BFGS-B (Defaults to 5)
    #                factr, # <-- L-BFGS-B (Default is 1e7, that is a tolerance of about 1e-8)
    #                pgtol, # <-- L-BFGS-B (Defaults to 0)
    #                temp, tmax)
    if(is.null(control$trace)){
      control$trace <- 0
    }
    if(is.null(control$maxit)){
      control$maxit <- 9999
    }
    if(is.null(control$fnscale)){
      control$fnscale = 0.001
    }
    if(is.null(control$lmm)){
      control$lmm = 7
    }

    arg <- list(
      par = initial_eta,
      fn = compute_ofv,
      method = method,
      control = control,
      lower = bound,
      upper = -bound,
      select_eta = select_eta
    )
  }

  return(arg)
}


#' Preprocess model and data for ofv computation
#'
#' @name preprocess.ofv
#' @param x the model object
#' @param data,iddata NMTRAN-like data set. iddata is likely a dataset of one individual
#' @return A list of arguments used to compute the objective function value.
#'
#' The following arguments are fixed between individuals:
#'
#'  - `qmod`: model object, modified to simulate without random effects and with controlled outputs
#'  - `sigma`: a single matrix object
#'  - `log_transformation`: a logical, whether predictions need to be log-transformed for ofv computation
#'  - `omega_inv`: a single matrix object
#'  - `all_cmt`: a vector of compartment numbers where observations can be expected
#'
#' The following arguments differs between individuals:
#'
#'  - `idvaliddata`: a matrix, individual data set (with administrations and covariates), validated with \code{\link[mrgsolve]{valid_data_set}}
#'  - `idDV`: a vector of (possibly log-transformed) observations
#'  - `idcmt`: a vector of compartments where observations belong to
#'
#' @examples
#' mod <- exmodel(add_exdata = FALSE, compile = FALSE)
#' dat <- exdata(ID = c(1,4))
#'
#' preprocess.ofv.fix(x = mod, data = dat)
#' preprocess.ofv.id(x = mod, iddata = dat[dat$ID == 1,])
#' preprocess.ofv.id(x = mod, iddata = dat[dat$ID == 4,])
#'
#' @description Functions to generate arguments passed to \code{\link{compute_ofv}}. Arguments that are fixed between individuals are created once (`preprocess.ofv.fix`), while others are specific of each individual (`preprocess.ofv.id`).
NULL
#> NULL

#' Preprocess fix arguments for ofv computation
#' @rdname preprocess.ofv
#' @export
preprocess.ofv.fix <- function(x, data){
  qmod <- zero_re(x)
  qmod@end <- -1 #Make sure no modif in the time grid
  qmod@cmtL <- character(0) # Do not return amounts in compartments in the output
  qmod@Icmt <- integer(0)
  qmod@Icap <- which(x@capL== "DV") # Only return DV among $captured items
  qmod@capL <- "DV"

  list(
    qmod = qmod,
    sigma = smat(x, make = T),
    log_transformation = log_transformation(x),
    omega_inv = solve(omat(x, make = T)),
    all_cmt = fit_cmt(x, data) #on full data
  )
}

#' Preprocess individual arguments for ofv computation
#' @rdname preprocess.ofv
#' @export
preprocess.ofv.id <- function(x, iddata){
  # --- Checks id data vs model

  #eg : at least one obs per id

  # --- Generate preprocess

  idDV <- iddata$DV[iddata$mdv==0] #keep observations to fit only
  if(log_transformation(x)) idDV <- log(idDV)
  idcmt <- iddata$cmt[iddata$mdv==0]

  list(idvaliddata = valid_data_set(iddata, x),
       idDV = idDV,
       idcmt = idcmt
  )
}
