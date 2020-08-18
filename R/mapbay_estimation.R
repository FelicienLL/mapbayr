#' Perform mapbay estimation from NM-TRAN data and mapbay model
#'
#' @param data a dataframe, dataset (NM-TRAN format) of one individual to fit
#' @param model a mapbay_model list, with the characteristics of the model, including a compiled mrgsolve_model
#' @param output_df If TRUE, output = dataframe is returned and not a list of components
#' @param force_initial_eta a numeric vector of starting estimates (exact length of eta to estimate )
#'
#' @return default: a list with initial eta, newuoa outputs, final eta and a NM-like dataset with predictions
#' @export
#'
#' @import mrgsolve
#'
#' @importFrom dplyr filter as_tibble mutate select right_join select starts_with rename_with any_of distinct
#' @importFrom magrittr %>%
#' @importFrom minqa newuoa
#' @importFrom rlang .data set_names
#' @importFrom stats runif
#' @importFrom stringr str_c
#'
mapbay_estimation <- function(data, model, output_df = F, force_initial_eta = NULL){

  data <- data %>%
    rename_with(tolower, any_of(c("TIME", "AMT", "MDV", "CMT", "EVID", "II", "ADDL", "SS", "RATE")))

  n_omega <- length(diag(model$param_omega_matrix))

  if(is.null(force_initial_eta)){
    initial_eta <- runif(n_omega, -0.5, 0.5) %>% magrittr::set_names(str_c("ETA", 1:n_omega))
  } else {
    initial_eta <- force_initial_eta %>% magrittr::set_names(str_c("ETA", 1:n_omega))
  }

  data_to_fit <- data %>%
    filter(!(.data$evid==0&.data$mdv==1))

  omega.inv <- solve(model$param_omega_matrix)

  DVobs <- data_to_fit[data_to_fit$evid==0,]$DV

  f_compute_ofv <- ifelse(length(model$obs_cmt)>1,
                          compute_ofv_m,
                          compute_ofv)

  newuoa_value <- newuoa(
    initial_eta,
    f_compute_ofv,
    data  = data_to_fit,
    mrgsolve_model = model$mrgsolve_model,
    sigma = model$param_sigma_matrix,
    log.transformation = model$log.transformation,
    DVobs = DVobs,
    omega.inv = omega.inv,
    obs_cmt = model$obs_cmt,
    control = list(iprint = 2, maxfun = 50000))

  final_eta <- newuoa_value$par %>% magrittr::set_names(str_c("ETA", 1:n_omega))


  carry <- data %>%
    select(-any_of(c("ID", "time","DV"))) %>%
    names()


  mapbay_tab <- model$mrgsolve_model %>%
    param(final_eta) %>%
    data_set(data) %>%
    zero_re() %>%
    mrgsim(carry_out = carry) %>%
    as_tibble() %>%
    mutate(IPRED = .data$DV) %>%
    mutate(DV = data$DV)

  if(output_df){

    mapbay_output <- mapbay_tab

  } else {

    mapbay_output <- list(
      initial_eta  = initial_eta,
      newuoa_value = newuoa_value,
      final_eta    = final_eta,
      mapbay_tab   = mapbay_tab )

  }

  return(mapbay_output)

}
