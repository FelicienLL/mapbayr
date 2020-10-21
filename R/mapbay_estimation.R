#' Perform mapbayesian estimation from a mrgsolve model and a NM_tran dataset
#'
#' @param x model file
#' @param data NM tran data to optimize
#' @param output return a mapbay_tab only
#' @param force_initial_eta a numeric vector of starting estimates (exact length of eta to estimate )
#'
#' @return default: a list with data, model, initial eta, newuoa outputs, final eta, and mapbay_tab
#' @export
#'
mbrest <- function(x, data = NULL, output = NULL, force_initial_eta = NULL){
  if(is.null(data)){
    data <- x@args$data
  }

  if(length(unique(data$ID)) != 1) stop("Only one individual at a time (consider apply or map)")

  data0 <- data

  data <- data %>%
    rename_with(tolower, any_of(c("TIME", "AMT", "MDV", "CMT", "EVID", "II", "ADDL", "SS", "RATE"))) %>%
    mutate(evid = ifelse(.data$evid == 0, 2, .data$evid))

  pre <- preprocess(data = data, model = x, force_initial_eta = force_initial_eta)

  newuoa_value <- do.call(newuoa, pre)

  post <- postprocess(data = data, model = x, newuoa_value = newuoa_value, data0 = data0, pre = pre)

  mapbay_output <- post
  if(!is.null(output)){
    if(output == "df") mapbay_output <- post$mapbay_tab
  }

  return(mapbay_output)
}



#' Preprocess model and data for optimization
#'
#' @param model a compiled mrgsolve_model
#' @param data a dataframe, dataset (NM-TRAN format) of one individual to fit
#' @param force_initial_eta a numeric vector of starting estimates (exact length of eta to estimate )
#'
#' @return a data.frame ready for mapbay_estimation
#' @export
preprocess <- function(model, data, force_initial_eta = NULL){

  n_omega <- length(diag(omat(model, make = T)))
  omega.inv <- solve(omat(model, make = T))
  sigma <- smat(model, make = T)
  if(is.null(force_initial_eta)){
    initial_eta <- runif(n_omega, -0.5, 0.5) %>% set_names(str_c("ETA", 1:n_omega))
  } else {
    initial_eta <- force_initial_eta %>% set_names(str_c("ETA", 1:n_omega))
  }

  if(nrow(data %>% filter(.data$time == 0, .data$mdv ==0)) > 0) stop("Observation line (mdv = 0) not accepted at t0 (time = 0)")

  data_to_fit <- data %>%
    filter(!(.data$evid%in%c(0,2)&.data$mdv==1))

  model <- model %>%
    zero_re() %>%
    data_set(data_to_fit)

  DVobs <- data_to_fit[data_to_fit$evid%in%c(0,2),]$DV
  if(log.transformation(model)){DVobs <- log(DVobs)}

  list(par = initial_eta,
       fn  = compute_ofv,
       mrgsolve_model = model,
       sigma = sigma,
       log.transformation = log.transformation(model),
       DVobs = DVobs,
       omega.inv = omega.inv,
       obs_cmt = obs_cmt(model),
       control = list(iprint = 2, maxfun = 50000)
  )
}







#' Post process results from optimization
#'
#' @param data data passed through processing
#' @param model a compiled mrgsolve_model
#' @param newuoa_value output returned by newuoa (list of length 4)
#' @param pre preprocessed object
#' @param data0 original data
#'
#' @return a list of post processing values
#' @export
postprocess <- function(data, model, newuoa_value, data0, pre){

  final_eta <- newuoa_value$par %>% set_names(names(pre$par))

  if(is.nan(newuoa_value$fval)) {
    final_eta <- rep(0, length(diag(omat(model, make = T)))) %>% set_names(names(pre$par))
    warning("Cannot compute objective function value ; typical value (ETA = 0) returned")
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
    mutate(evid = data0$evid) %>%
    select(-any_of(model@cmtL))

  list(
    data = data,
    model = model,
    initial_eta  = pre$par,
    newuoa_value = newuoa_value,
    final_eta    = final_eta,
    mapbay_tab   = mapbay_tab
  )

}
