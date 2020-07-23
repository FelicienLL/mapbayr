
#' Title
#'
#' @param data a dataframe, NM-TRAN like
#'
#' @return a dataframe, NM-TRAN like (except colnames)
#' @export
#'
#' @importFrom rlang .data
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate across any_of rename rename_all
#'
#'
print_data_tki <- function(data){

  data %>%
    mutate(across(any_of(c('ID', "evid", "amt","cmt","addl","ii", "mdv", "rate")), as.integer)) %>%
    rename_all(toupper) %>%
    rename("TIME (h)"  = .data$TIME,
           "AMT (mg)"  = .data$AMT,
           "II (h)"    = .data$II,
           "DV (mg/L)" = .data$DV)
}

