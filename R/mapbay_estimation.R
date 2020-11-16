#' Perform mapbayesian estimation from a mrgsolve model and a NM_tran dataset
#'
#' @param x model file
#' @param data NM tran data to optimize
#' @param method "newuoa" or "L-BFGS-B"
#' @param output return a mapbay_tab only
#' @param control a list passed to the optimizer (see source code for default, as function of the optimizer)
#' @param force_initial_eta a numeric vector of starting estimates (exact length of eta to estimate )
#' @param quantile_bound for L-BFGS-B only: a numeric value of the probability expected as extreme value for a ETA
#'
#' @return default: a list with data, model, initial and final eta, mapbay_tab and rough optimization output
#' @export
#'
mbrest <- function(x, data = NULL, method = "newuoa", output = NULL, control = NULL, force_initial_eta = NULL, quantile_bound = 0.001){
  if(is.null(data)){
    data <- x@args$data
  }

  okmethod <- c("newuoa", "L-BFGS-B")

  if(length(unique(data$ID)) != 1) stop("Only one individual at a time (consider apply or map)")
  if(!method %in% okmethod) stop(paste("Accepted methods:", paste(okmethod, collapse = ", ")))

  data <- data %>%
    rename_with(tolower, any_of(c("TIME", "AMT", "MDV", "CMT", "EVID", "II", "ADDL", "SS", "RATE")))

  fn.optim <- switch(method,
                     "newuoa" = newuoa,
                     "L-BFGS-B" = stats::optim)

  arg.optim <- preprocess.optim(method = method, model = x, control = control, force_initial_eta = force_initial_eta, quantile_bound = quantile_bound)
  arg.ofv <- preprocess.ofv(data = data, model = x)

  opt.value <- do.call(fn.optim, c(arg.optim, arg.ofv))

  post <- postprocess(data = data, model = x, opt.value = opt.value, arg.optim = arg.optim, arg.ofv = arg.ofv)

  mapbay_output <- post
  if(!is.null(output)){
    if(output == "df") mapbay_output <- post$mapbay_tab
  }

  return(mapbay_output)
}



#' Preprocess model and data for ofv computation
#'
#' @param model a compiled mrgsolve_model
#' @param data a dataframe, dataset (NM-TRAN format) of one individual to fit
#'
#' @return a list of argument passed to optimization function
#' @export
preprocess.ofv <- function(model, data){

  n_omega <- length(diag(omat(model, make = T)))
  omega.inv <- solve(omat(model, make = T))
  sigma <- smat(model, make = T)

  if(nrow(data %>% filter(.data$time == 0, .data$mdv ==0)) > 0) stop("Observation line (mdv = 0) not accepted at t0 (time = 0)")

  data_to_fit <- data %>%
    filter(!(.data$evid%in%c(0,2)&.data$mdv==1))

  model <- model %>%
    zero_re() %>%
    data_set(data_to_fit)

  DVobs <- data_to_fit[data_to_fit$evid%in%c(0,2),]$DV
  if(log.transformation(model)){DVobs <- log(DVobs)}

  list(
    mrgsolve_model = model,
    sigma = sigma,
    log.transformation = log.transformation(model),
    DVobs = DVobs,
    omega.inv = omega.inv,
    obs_cmt = obs_cmt(model)
  )
}





#' Preprocess: arguments for optimization function
#'
#' @param method string character of method to use ("newuoa" or "L-BFGS-B")
#' @param model model object
#' @param control a list passed to the optimizer
#' @param force_initial_eta a numeric vector of starting estimates (exact length of eta to estimate)
#' @param quantile_bound for L-BFGS-B only: a numeric value of the probability expected as extreme value for a ETA
#'
#' @return a list of argument passed to optimization function
#' @export
preprocess.optim <- function(method, model, control, force_initial_eta, quantile_bound){
  diag_omega <- diag(omat(model, make = T))

  if(method == "newuoa"){
    initial_eta <- force_initial_eta
    if(is.null(force_initial_eta)){
      set.seed(1)
      initial_eta <- runif(length(diag_omega), -0.01, 0.01)  %>% set_names(str_c("ETA", 1:length(diag_omega)))
    }
    if(is.null(control)){
      control <- list(iprint = 2, maxfun = 50000)
    }

    arg <- list(
      par = initial_eta,
      fn = compute_ofv,
      control = control
    )
  }

  if(method == "L-BFGS-B"){
    initial_eta <- force_initial_eta
    if(is.null(force_initial_eta)){
      initial_eta <- rep(0,length(diag_omega)) %>% set_names(str_c("ETA", 1:length(diag_omega)))
    }
    if(is.null(control)){
      control <- list(fnscale = 0.001, trace = 1, maxit = 2000, lmm = 7)
    }
    bound <- map_dbl(sqrt(diag(omat(model, make= T))), qnorm, p = quantile_bound, mean = 0)

    arg <- list(
      par = initial_eta,
      fn = compute_ofv,
      method = "L-BFGS-B",
      control = control,
      lower = bound,
      upper = -bound
    )
  }

  return(arg)
}



#' Post process results from optimization
#'
#' @param data data passed through processing
#' @param model a compiled mrgsolve_model
#' @param opt.value value obtained by optimization function
#' @param arg.optim argument passed to optimization function
#' @param arg.ofv argument passed to optimization function
#'
#' @return a list of post processing values
#' @export
postprocess <- function(data, model, opt.value, arg.optim, arg.ofv){
  final_eta <- opt.value$par %>% set_names(names(arg.optim$par))

  if(!is.null(opt.value$fval)){
    if(is.nan(opt.value$fval)) {
      final_eta <- rep(0, length(diag(omat(model, make = T)))) %>% set_names(names(arg.ofv$par))
      warning("Cannot compute objective function value ; typical value (ETA = 0) returned")
    }
  }

  carry <- data %>%
    select(-any_of(c("ID", "time","DV"))) %>%
    names()

  typical_pred <- model %>%
    data_set(data) %>%
    zero_re() %>%
    mrgsim(carry_out = carry, end = -1) %>%
    as_tibble() %>%
    pull(.data$DV)

  mapbay_tab <- model %>%
    param(final_eta) %>%
    data_set(data) %>%
    zero_re() %>%
    mrgsim(carry_out = carry, end = -1) %>%
    as_tibble() %>%
    mutate(IPRED = .data$DV, PRED = typical_pred, .after = "DV") %>%
    mutate(DV = data$DV) %>%
    select(-any_of(model@cmtL))

  list(
    data = data,
    model = model,
    arg.optim = arg.optim,
    arg.ofv = arg.ofv,
    opt.value = opt.value,
    final_eta = final_eta,
    mapbay_tab = mapbay_tab
  )

}
