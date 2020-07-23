#' Title
#'
#' @param estimates a list, default output of the mapbay_estimation
#' @param model a mapbay_model list, with the characteristics of the model, including a compiled mrgsolve_model
#' @param covariates  a list of named covariates, with a single value or exact number of lines than data
#' @param sim_amt numeric input
#' @param sim_ii numeric input
#'
#' @return a dataframe, tibble with 5th 50th and 95th percentile of simulation
#' @export
#' @import mrgsolve
#' @importFrom dplyr filter mutate select arrange as_tibble group_by summarise
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @importFrom stats quantile
#' @importFrom tidyr crossing
#'
compute_simulation <- function(estimates, model, covariates, sim_amt, sim_ii){
  ds <- adm_lines(model,
                   addl = 15*(24/sim_ii),
                   ii = sim_ii,
                   amt = sim_amt) %>%
    add_rate_column(model = model) %>%
    add_covariates(covariates = covariates, model = model) %>%
    select(-.data$ID) %>%
    crossing(ID = c(1:1000)) %>%
    arrange(.data$ID, .data$time, .data$evid)


  set.seed(20200117)

  model$mrgsolve_model %>%
    data_set(ds) %>%
    param(estimates$final_eta) %>%
    zero_re() %>%
    smat(model$param_sigma_matrix) %>%
    mrgsim_df(end = 24*(18), delta = 2) %>%
    as_tibble() %>%
    mutate(TIME = .data$time) %>%
    select(-.data$time) %>%
    group_by(.data$TIME) %>%
    summarise(p05 = quantile(.data$DV, 0.05),
              p50 = quantile(.data$DV, 0.5),
              p95 = quantile(.data$DV, 0.95),
              DOSING = str_c(sim_amt, "mg/", sim_ii,"h")
    ) #%>%
  # mutate(Ctrough_min = last(p05),
  #        Ctrough_med = last(p50),
  #        Ctrough_max = last(p95))
}

