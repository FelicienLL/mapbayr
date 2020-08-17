#' Title
#'
#' @param data a dataframe, dataset (NM-TRAN format) of one individual to fit
#' @param model a mapbay_model list, with the characteristics of the model, including a compiled mrgsolve_model
#' @param output a string. If output = "df" a dataframe is returned and not a list of component. Useful for mapping multiple individual datasets
#' @param sigdig a positive integer (cf focei nlmixr for significance)
#' @param control_optim list of argument to be passed to optim function
#' @param force_initial_eta a numeric vector of starting estimates (exact length of eta to estimate )
#' @param force_rtol change rtol of the mrgsolve model
#'
#' @return default: a list with initial eta, newuoa outputs, final eta and a NM-like dataset with predictions
#' @export
#'
#' @import mrgsolve
#'
#' @importFrom dplyr filter as_tibble mutate select right_join select starts_with rename_with any_of distinct
#' @importFrom magrittr %>%
#' @importFrom rlang .data set_names
#' @importFrom stats runif optim
#'
mapbay_estimation2 <- function(data, model, output = "default", force_initial_eta = NULL, sigdig = 3, force_rtol = -8,
                               control_optim = list(maxit = 50000, pgtol = 10^-sigdig, trace = 6)){

  iID <- unique(data$ID)

  if(length(iID) != 1) stop("n ID != 1. Please consider 'map'") else cat(paste0("\nID: ",iID, "\n"))

  model$mrgsolve_model@rtol <- 10^force_rtol

  data <- rename_with(data, tolower, any_of(c("TIME", "AMT", "MDV", "CMT", "EVID", "II", "ADDL", "SS", "RATE")))

  n_omega <- length(diag(model$param_omega_matrix))

  if(is.null(force_initial_eta)){
    initial_eta <- runif(n_omega, -0.5, 0.5) %>% set_names(paste0("ETA", 1:n_omega))
  } else {
    initial_eta <- force_initial_eta %>% set_names(paste0("ETA", 1:n_omega))
  }

  data_to_fit <- filter(data, !(.data$evid==0&.data$mdv==1))

  omega.inv <- solve(model$param_omega_matrix)

  DVobs <- data_to_fit[data_to_fit$evid==0,]$DV

  f_compute_ofv <- ifelse(length(model$obs_cmt)>1,
                          compute_ofv_m,
                          compute_ofv)

  list.args.computeofv <- list(
    data = data_to_fit,
    mrgsolve_model = model$mrgsolve_model,
    sigma = model$param_sigma_matrix,
    log.tranformation = model$log.tranformation,
    DVobs = DVobs,
    omega.inv = omega.inv,
    obs_cmt = model$obs_cmt
  )

  list.args.optim <- list(
    par = initial_eta,
    fn = f_compute_ofv,
    method = 'L-BFGS-B',
    control = control_optim
  )

  optim_value <- do.call(optim, c(list.args.optim, list.args.computeofv))

  final_eta <- optim_value$par %>% set_names(paste0("ETA", 1:n_omega))

  mapbay_tab <- model$mrgsolve_model %>%
    param(final_eta) %>%
    data_set(data) %>%
    zero_re() %>%
    mrgsim_df(carry_out = c("evid", "mdv", "cmt")) %>%
    as_tibble() %>%
    mutate(IPRED = .data$DV) %>%
    select(any_of(c("ID", "time", "evid", "cmt", "mdv", "IPRED", "PAR", "MET")), starts_with("ETA")) %>%
    right_join(data, by = c("ID", "time", "evid", "cmt", "mdv"))%>%
    select(any_of(c("ID", "time", "evid", "cmt", "mdv", "amt", "addl", "ii", "DV", "PAR", "MET", "IPRED", "DOSE")), starts_with('ETA'))

  if(output == "df"){

    mapbay_output <- mapbay_tab

  } else {

    mapbay_output <- list(
      initial_eta  = initial_eta,
      optim_value  = optim_value,
      final_eta    = final_eta,
      mapbay_tab   = mapbay_tab )

  }

  return(mapbay_output)

}



#' Title
#'
#' @param data a dataframe, dataset (NM-TRAN format) of one individual to fit
#' @param model a mapbay_model list, with the characteristics of the model, including a compiled mrgsolve_model
#' @param output a string. If output = "df" a dataframe is returned and not a list of component. Useful for mapping multiple individual datasets
#' @param force_initial_eta a numeric vector of starting estimates (exact length of eta to estimate )
#' @param force_rtol change rtol of the mrgsolve model
#' @param control_nloptr list of argument to be passed to nloptr function
#'
#' @return default: a list with initial eta, newuoa outputs, final eta and a NM-like dataset with predictions
#' @export
#'
#' @import mrgsolve
#' @importFrom nloptr nloptr
#' @importFrom dplyr filter as_tibble mutate select right_join select starts_with rename_with any_of distinct
#' @importFrom magrittr %>%
#' @importFrom rlang .data set_names
#' @importFrom stats runif
mapbay_estimation3 <- function(data, model, output = "default", force_initial_eta = NULL,
                               force_rtol = -8,
                               control_nloptr = list(
                                 algorithm = 'NLOPT_LN_NEWUOA',
                                 maxeval = 50000,
                                 print_level = 1
                               )){

  iID <- unique(data$ID)

  if(length(iID) != 1) stop("n ID != 1. Please consider 'map'") else cat(paste0("\nID: ",iID, "\n"))

  model$mrgsolve_model@rtol <- 10^force_rtol

  data <- rename_with(data, tolower, any_of(c("TIME", "AMT", "MDV", "CMT", "EVID", "II", "ADDL", "SS", "RATE")))

  n_omega <- length(diag(model$param_omega_matrix))

  if(is.null(force_initial_eta)){
    initial_eta <- runif(n_omega, -0.5, 0.5) %>% set_names(paste0("ETA", 1:n_omega))
  } else {
    initial_eta <- force_initial_eta %>% set_names(paste0("ETA", 1:n_omega))
  }

  data_to_fit <- filter(data, !(.data$evid==0&.data$mdv==1))

  omega.inv <- solve(model$param_omega_matrix)

  DVobs <- data_to_fit[data_to_fit$evid==0,]$DV

  f_compute_ofv <- ifelse(length(model$obs_cmt)>1,
                          compute_ofv_m,
                          compute_ofv)

  list.args.computeofv <- list(
    data = data_to_fit,
    mrgsolve_model = model$mrgsolve_model,
    sigma = model$param_sigma_matrix,
    log.tranformation = model$log.tranformation,
    DVobs = DVobs,
    omega.inv = omega.inv,
    obs_cmt = model$obs_cmt
  )

  browser()
  nloptr_value <- do.call(nloptr, c(list(x0 = initial_eta, eval_f = f_compute_ofv, opts = control_nloptr), list.args.computeofv))

  final_eta <- nloptr_value$solution %>% set_names(paste0("ETA", 1:n_omega))

  mapbay_tab <- model$mrgsolve_model %>%
    param(final_eta) %>%
    data_set(data) %>%
    zero_re() %>%
    mrgsim_df(carry_out = c("evid", "mdv", "cmt")) %>%
    as_tibble() %>%
    mutate(IPRED = .data$DV) %>%
    select(any_of(c("ID", "time", "evid", "cmt", "mdv", "IPRED", "PAR", "MET")), starts_with("ETA")) %>%
    right_join(data, by = c("ID", "time", "evid", "cmt", "mdv"))%>%
    select(any_of(c("ID", "time", "evid", "cmt", "mdv", "amt", "addl", "ii", "DV", "PAR", "MET", "IPRED", "DOSE")), starts_with('ETA'))

  if(output == "df"){

    mapbay_output <- mapbay_tab

  } else {

    mapbay_output <- list(
      initial_eta  = initial_eta,
      nloptr_value  = nloptr_value,
      final_eta    = final_eta,
      mapbay_tab   = mapbay_tab )

  }

  return(mapbay_output)

}
