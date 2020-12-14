#' Preprocess: data into a list of individual data
#'
#' @param data a NM-TRAN data set of one or multiple individuals.
#'
#' @return a named list of n data set (n individuals)
#' @export
preprocess.data <- function(data){
  if(is.null(data)) "No data provided"

  data <- data %>%
    rename_with(tolower, any_of(c("TIME", "AMT", "MDV", "CMT", "EVID", "II", "ADDL", "SS", "RATE")))

  iID <- unique(data$ID)

  idata <- data %>%
    mutate(split_ID = factor(.data$ID, levels = iID)) %>%
    group_by(.data$split_ID) %>%
    group_split(.keep = FALSE) %>%
    set_names(iID)

  return(idata)
}



#' Preprocess model and data for ofv computation
#'
#' @param model a compiled mrgsolve_model
#' @param data a dataframe, dataset (NM-TRAN format)
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
    mrgsolve_model = model, #this model is 'updated' with zero re and include the data adm
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
#' @return a list of argument passed to optimization function (optimx)
#' @export
preprocess.optim <- function(method, model, control, force_initial_eta, quantile_bound){
  #Checks argument

  #method
  okmethod <- c("newuoa", "L-BFGS-B")
  if(!method %in% okmethod) stop(paste("Accepted methods:", paste(okmethod, collapse = ", "), '.'))
  method <- method[1]

  diag_omega <- diag(omat(model, make = T))
  eta_names <- str_c("ETA", 1:length(diag_omega))

  #par
  initial_eta <- force_initial_eta
  if(is.null(initial_eta)){
    if(method == "newuoa"){
      set.seed(1)
      initial_eta <- runif(length(diag_omega), -0.01, 0.01)  %>% set_names(str_c("ETA", 1:length(diag_omega)))
    }
    if(method == "L-BFGS-B"){
      initial_eta <- rep(0,length(diag_omega)) %>% set_names(str_c("ETA", 1:length(diag_omega)))
    }

  }

  #fn = compute_ofv

  #control
  if(is.null(control$trace)){
    control <- c(control, list(trace = 0))
  }
  if(is.null(control$kkt)){
    control <- c(control, list(kkt = FALSE))
  }
  if(method == "L-BFGS-B"){
    if(is.null(control$fnscale))
      control <- c(control, list(fnscale = 0.001))
    if(is.null(control$lmm))
      control <- c(control, list(lmm = 7))
  }

  #lower, upper
  bound = Inf
  if(method == "L-BFGS-B"){
    bound <- map_dbl(sqrt(diag_omega), qnorm, p = quantile_bound, mean = 0)
  }

  arg <- list(
    par = initial_eta,
    fn = compute_ofv,
    method = method,
    control = control,
    lower = bound,
    upper = -bound
  )

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

  final_eta <- opt.value[names(arg.optim$par)] %>%
    as.double() %>%
    set_names(names(arg.optim$par))

  if(!is.null(opt.value$fevals)){
    if(is.nan(opt.value$fevals)) {
      final_eta <- rep(0, length(diag(omat(model, make = T)))) %>% set_names(names(arg.optim$par))
      warning("Cannot compute objective function value ; typical value (ETA = 0) returned")
    }
  }

  typical_pred <- model %>%
    data_set(data) %>%
    zero_re() %>%
    mrgsim(end = -1) %>%
    as_tibble() %>%
    pull(.data$DV)


  indiv_pred <- model %>%
    param(final_eta) %>%
    data_set(data) %>%
    zero_re() %>%
    mrgsim(end = -1) %>%
    as_tibble() %>%
    pull(.data$DV)

  mapbay_tab <- data %>%
    mutate(IPRED = indiv_pred, PRED = typical_pred, .after = "DV") %>%
    select(-any_of(model@cmtL)) %>%
    bind_cols(bind_rows(final_eta))

  list(
    #  data = data,
    #  model = model,
    #  arg.optim = arg.optim,
    #  arg.ofv = arg.ofv,
    #  opt.value = opt.value,
    final_eta = final_eta,
    mapbay_tab = mapbay_tab
  )

}


#' Build the output of mbrest function
#' @param idata data passed through processing
#' @param model a compiled mrgsolve_model
#' @param arg.optim argument passed to optimization function
#' @param arg.ofv argument passed to optimization function
#' @param opt.value value obtained by optimization function
#' @param post output of the post.process function
#' @param output return a mapbay_tab only
#'
#' @return a mbrests list object
#' @export
output_mbr <- function(idata, model, arg.optim, arg.ofv, opt.value, post, output){

  if(!is.null(output)){
    if(output == "df") out <- map_dfr(post, "mapbay_tab")

  } else {
    out <- list(
      data = bind_rows(unname(idata)),
      model = model,
      arg.optim = arg.optim,
      arg.ofv = arg.ofv,
      opt.value = map_dfr(opt.value, rownames_to_column, var = "method", .id = "ID"),
      final_eta = map(post, "final_eta"),
      mapbay_tab = map_dfr(post, "mapbay_tab")
    )

    class(out) <- "mbrests"

  }
  return(out)

}
