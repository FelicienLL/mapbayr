#' Title
#'
#' @param data a dataframe, NM-TRAN like
#' @param estimates a list, default output of the mapbay_estimation
#' @param model a mapbay_model list, with the characteristics of the model, including a compiled mrgsolve_model
#' @param time_target a numeric, time needed for reestimation of concentration
#' @param choose_delta a numeric, delta for simulation process (default = .1)
#'
#' @return a dataframe, a mrgsolve 'df' output
#' @export
#' @import mrgsolve
#' @importFrom dplyr as_tibble
#' @importFrom magrittr %>%
#'
compute_prediction <- function(data, estimates, model, time_target, choose_delta = .1){

  t_end = max(max(data[["time"]]), time_target) # if sampling time > to theoretical trough time

  model$mrgsolve_model %>%
    param(estimates[["final_eta"]]) %>%
    zero_re %>%
    data_set(data) %>%
    obsaug() %>%
    mrgsim_df(end = t_end, delta = choose_delta, carry_out = c("evid", "cmt", "mdv")) %>%
    as_tibble()

}
