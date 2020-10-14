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

  data0 <- data

  data <- data %>%
    rename_with(tolower, any_of(c("TIME", "AMT", "MDV", "CMT", "EVID", "II", "ADDL", "SS", "RATE"))) %>%
    mutate(evid = ifelse(.data$evid == 0, 2, .data$evid))

  pre <- preprocess(data = data, model = model, force_initial_eta = force_initial_eta)

  newuoa_value <- do.call(newuoa, pre)

  post <- postprocess(data = data, model = model, newuoa_value = newuoa_value, data0 = data0, pre = pre)

  mapbay_output <- post
  if(output_df) mapbay_output <- post$mapbay_tab

  return(mapbay_output)

}


#' Title
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
  data0 <- data

  data <- data %>%
    rename_with(tolower, any_of(c("TIME", "AMT", "MDV", "CMT", "EVID", "II", "ADDL", "SS", "RATE"))) %>%
    mutate(evid = ifelse(.data$evid == 0, 2, .data$evid))

  pre <- preprocess2(data = data, model = x, force_initial_eta = force_initial_eta)

  newuoa_value <- do.call(newuoa, pre)

  post <- postprocess2(data = data, model = x, newuoa_value = newuoa_value, data0 = data0, pre = pre)

  mapbay_output <- post
  if(!is.null(output)){
    if(output == "df") mapbay_output <- post$mapbay_tab
  }

  return(mapbay_output)
}
