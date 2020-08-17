#' Title
#'
#' @param model character name of the model in the mapbay model library of the package
#' @param path exact path to the model. Default = NULL (read in the library of mapbay models)
#'
#' @return a mapbay_model list, with the characteristics of the model, including a compiled mrgsolve_model
#' @export
#' @import mrgsolve
#' @importFrom magrittr %>%
#' @importFrom rlang set_names
#' @importFrom purrr map splice
#' @importFrom stringr str_subset str_remove str_squish str_split str_detect
#' @importFrom dplyr filter mutate right_join pull arrange
#' @importFrom tibble enframe
#' @importFrom tidyr replace_na
#'
load_mapbay_model <- function(model, path = NULL){

  if(is.null(path)){
    mrgsolve_model <- mread(system.file("mrg_models", paste0(model, "_mapbay.cpp"), package = "mapbayr"))
  } else{
    mrgsolve_model <- mread(model = path)
  }
  mapbay_model <- list(
    mrgsolve_model = mrgsolve_model
  )

  #Character of length 1

  mapbay_model <- c("drug", "model_name", "model_ref", "error_model", "concentration_unit") %>%
    set_names(c("drug", "model_name", "model_ref", "error_model", "concentration_unit")) %>%
    map(mrgsolve_model = mrgsolve_model,
        .f = function(.x, mrgsolve_model){
          pat <- paste0("\\s*-\\s*",.x, "\\s*:\\s*")
          mrgsolve_model@code %>%
            str_subset(pat) %>%
            str_remove(pat) %>%
            str_squish()
        }) %>%
    splice(mapbay_model)

  #Numeric vector

  mapbay_model <- c("adm_cmt", "obs_cmt") %>%
    set_names(c("adm_cmt", "obs_cmt")) %>%
    map(mrgsolve_model = mrgsolve_model,
        .f = function(.x, mrgsolve_model){
          pat <- paste0("\\s*-\\s*",.x, "\\s*:\\s*")
          mrgsolve_model@code %>%
            str_subset(pat) %>%
            str_remove(pat)%>%
            str_squish() %>%
            str_split(",") %>%
            unlist() %>%
            str_squish() %>%
            as.numeric()
        }) %>%
    splice(mapbay_model)

  #Matrices

  mapbay_model$param_omega_matrix <- mrgsolve_model@omega@data$...
  mapbay_model$param_sigma_matrix <- mrgsolve_model@sigma@data$...

  #Other
  mapbay_model$model_file <- as.list(mrgsolve_model)$covariates%>%
    str_remove("_mapbay_cpp")

  mapbay_model$log.tranformation  <- str_detect(tolower(mapbay_model$error_model), "exp")
  mapbay_model$scaling_conc_from_user_to_model <- switch (mapbay_model$concentration_unit,
                                                          "mg/L" = 1,
                                                          "ng/mL"= 1000)
  mapbay_model$covariate_names <- as.list(mrgsolve_model)$covariates
  mapbay_model$covariate_ref_values <- unlist(mrgsolve_model@param[mapbay_model$covariate_names])
  mapbay_model$covariate_description <-  as.list(mrgsolve_model)$details$data%>%
    filter(.data$name %in% mapbay_model$covariate_names) %>%
    mutate(covariate_description = paste0(.data$descr, " (", .data$unit, ")")) %>%
    pull(.data$covariate_description)

  tab <- (as.list(mrgsolve_model))$details$data %>%
    filter(.data$block=="PARAM") %>%
    filter(str_detect(.data$name, "ETA"))

  mapbay_model$param_names <- tab$descr
  mapbay_model$param_units <- tab$unit
  mapbay_model$param_typical_values <- mrgsolve_model@param@data %>%
    unlist() %>%
    enframe() %>%
    mutate(descr = str_remove(.data$name, "TV"), .keep = "unused") %>%
    right_join(tab, by = "descr") %>%
    replace_na(list(value = 1)) %>%
    mutate(descr = factor(.data$descr, mapbay_model$param_names)) %>%
    arrange(.data$descr) %>%
    pull(.data$value)

  return(mapbay_model)

}
