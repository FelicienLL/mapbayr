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
    rename_with(tolower, any_of(c("TIME", "AMT", "MDV", "CMT", "EVID", "II", "ADDL", "SS", "RATE"))) %>%
    mutate(evid = ifelse(.data$evid == 0, 2, .data$evid))

  pre <- preprocess(data = data, model = model, force_initial_eta = force_initial_eta)

  newuoa_value <- do.call(newuoa, pre)

  final_eta <- newuoa_value$par %>% magrittr::set_names(names(pre$par))

  if(is.nan(newuoa_value$fval)) {
    final_eta <- rep(0, n_omega) %>% magrittr::set_names(names(pre$par))
    warning("Cannot compute objective function value ; typical value (ETA = 0) returned")
  }

  carry <- data %>%
    select(-any_of(c("ID", "time","DV"))) %>%
    names()

  mapbay_tab <- model$mrgsolve_model %>%
    param(final_eta) %>%
    data_set(data) %>%
    zero_re() %>%
    mrgsim(carry_out = carry, end = -1) %>%
    as_tibble() %>%
    mutate(IPRED = .data$DV, .after = "DV") %>%
    mutate(DV = data$DV)

  if(output_df){

    mapbay_output <- mapbay_tab

  } else {

    mapbay_output <- list(
      initial_eta  = pre$par,
      newuoa_value = newuoa_value,
      final_eta    = final_eta,
      mapbay_tab   = mapbay_tab )

  }

  return(mapbay_output)

}
