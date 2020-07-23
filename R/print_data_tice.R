#' Title
#'
#' @param input_data a dataframe, NM-TRAN like
#' @param estimates a list, default output of the mapbay_estimation
#'
#' @return a dataframe, NM-TRAN like
#' @export
#'
#' @importFrom rlang .data
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate select bind_cols rename_all mutate_at any_of
#'
print_data_tice <- function(input_data, estimates){

  if(is.na(estimates[1])){
    d <- list()
  } else {

  d <- estimates$mapbay_tab %>%
    mutate(DEV = scales::percent((.data$IPRED - .data$DV) / .data$DV)) %>%
    select(any_of(c("IPRED", "DEV")))

  }

  bind_cols(input_data, d) %>%
    rename_all(toupper) %>%
    mutate_at(c("ID","EVID", "MDV", "CMT"), as.integer) %>%
    select(-.data$ADDL, -.data$II)

}
