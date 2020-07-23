#' Title
#'
#' @param data a dataframe, NM-TRAN like
#' @param sim_amt numeric input
#' @param sim_ii numeric input
#'
#' @return a dataframe, NM-TRAN like
#' @export
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @importFrom dplyr filter mutate select arrange
#' @importFrom tidyr crossing
#'
build_simulation_dataset <- function(data, sim_amt, sim_ii){

  data %>%
    filter(.data$evid == 1) %>%
    mutate(addl = 15*(24/sim_ii)-1,
           ii   = sim_ii,
           amt  = sim_amt,
           DOSE = .data$amt) %>%
    select(-.data$ID, -.data$DV) %>%
    crossing(ID = c(1:1000)) %>%
    arrange(.data$ID, .data$time, .data$evid)
}
