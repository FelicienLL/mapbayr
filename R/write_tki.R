#' Title
#'
#' @param pred_data a dataframe, NM-TRAN like
#' @param estimates a list, default output of the mapbay_estimation
#' @param model a mapbay_model list, with the characteristics of the model, including a compiled mrgsolve_model
#' @param time_last_dose numeric input
#' @param time_target numeric input
#'
#' @return a character string
#'
#' @importFrom rlang .data
#' @importFrom magrittr %>%
#' @importFrom dplyr filter slice pull
#' @importFrom stringr str_c
#' @importFrom scales percent
#' @importFrom purrr map_dbl
#'
#' @export
#'
#'
write_comment_fit_tki <- function(pred_data, estimates, model, time_last_dose, time_target){

  scaling <- model$scaling_conc_from_user_to_model

  time_sample <- estimates$mapbay_tab %>%
    filter(.data$evid==0) %>%
    #   slice(1) %>%
    pull(.data$time)

  conc_obs_sample <- estimates$mapbay_tab %>%
    filter(.data$evid==0) %>%
    #    slice(1) %>%
    pull(.data$DV) #always mg/L

  conc_pred_sample <- estimates$mapbay_tab %>%
    filter(.data$time == time_sample) %>%
    #    slice(1) %>%
    pull(.data$IPRED) %>%
    signif(3) #en mg/L toujours

  pred_var <- if(length(conc_obs_sample) > 1){c("PAR", "MET")} else {"DV"}

  conc_pred_target <- map_dbl(pred_var, function(.x){
    pred_data %>%
      filter(.data$time == time_target) %>%
      pull(.data[[.x]]) %>%
      signif(3)
  })

  deviation <- function(obs = conc_obs_sample, pred = conc_pred_sample){
    err <- round((pred-obs)/obs, 2)

    if(err < 0){
      err2 <- scales::percent(err)
    }else {
      err2 <- str_c("+", scales::percent(err), sep = "")
    }

    err2
  }

  z <- if(length(conc_obs_sample) > 1){ c("PAR: ", "\nMET: ") } else ""

  a <- str_c("Concentration predite a ",
             time_sample-time_last_dose,
             " h : ",
             conc_pred_sample * scaling,
             model$concentration_unit
  )

  b <- str_c("Erreur ajustement :",
             deviation()
  )

  c <- str_c("Concentration residuelle estimee a H",
             time_target-time_last_dose,
             " : ",
             conc_pred_target * scaling,
             model$concentration_unit
  )

  abc <- str_c(z, a," ", b, "\n", c)

  abc
}


#' Title
#'
#' @param pred_data a dataframe, NM-TRAN like
#' @param model a list, characteristics of the model, including a compiled mrgsolve_model
#' @param interpretation_tki a list, characteristic of the drug, with therapeutic target
#' @param time_target numeric input
#'
#' @return a character string
#' @export
#' @importFrom rlang .data
#' @importFrom magrittr %>%
#' @importFrom dplyr filter slice pull
#' @importFrom stringr str_c
#'
#'
write_comment_biology_tki <- function(pred_data, model, interpretation_tki, time_target){

  scaling <- model$scaling_conc_from_user_to_model

  conc_pred_target <- pred_data %>%
    filter(.data$time == time_target) %>%
    slice(1) %>%
    pull(.data$DV) %>%
    signif(3)

  infsup <- ifelse(conc_pred_target * scaling < interpretation_tki$concentration_target, "inferieure", "superieure")

  str_c("Commentaire MOLIS : \nConcentration reestimee par analyse bayesienne sur la base d'un modele de pharmacocinetique de population. (",
        model$model_ref,
        "). \nCette valeur de concentration est ",
        infsup,
        " a la valeur-cible de ",
        interpretation_tki$concentration_target,
        " ",
        model$concentration_unit,
        " recommandee (",
        interpretation_tki$target_ref,
        ').')
}

