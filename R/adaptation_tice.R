#' Title
#'
#' @param data a dataframe, NM-TRAN like
#' @param estimates a list, default output of the mapbay_estimation
#' @param model a mapbay_model list, with the characteristics of the model, including a compiled mrgsolve_model
#' @param auc_cible a numeric input
#'
#' @return a list with results of the adaptation according to Moeung et al, CCR, 2017
#' @importFrom tibble add_column
#' @importFrom dplyr bind_rows arrange
#' @importFrom tidyr fill
#'
#' @importFrom rlang .data
#' @importFrom magrittr %>%
#' @export
#'
adaptation_tice <- function(data, estimates, model, auc_cible){

  DOSE_D1   <- data[data$evid==1,]$amt
  CL        <- model$mrgsolve_model@param$TVCL * exp(unname(estimates$final_eta[1])) * 1000 / 60 # mL/min
  AUC_D1    <- DOSE_D1 / CL   # mg.mL/min
  DOSE_D3   <- CL * (auc_cible - (AUC_D1 * 2))

  if(DOSE_D3 < 0){
    DOSE_D3 <- 0
  }

  DOSE_C3D1 <- CL * 8

  input_pred <- adm_lines(model = model, time = c(24, 48), amt = c(DOSE_D1, DOSE_D3)) %>%
    add_column(rate = c(DOSE_D1, DOSE_D3)) %>%
    bind_rows(data) %>%
    arrange(.data$time) %>%
    fill(.data$BSA)

  list(
    CL        = CL,
    DOSE_D1   = DOSE_D1,
    AUC_D1    = AUC_D1,
    DOSE_D3   = DOSE_D3,
    DOSE_C3D1 = DOSE_C3D1,
    pred_data = compute_prediction(input_pred, estimates, model, 72)
  )

}
