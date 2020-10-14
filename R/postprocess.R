#' Title
#'
#' @param data data passed through processing
#' @param model a mapbay_model list, with the characteristics of the model, including a compiled mrgsolve_model
#' @param newuoa_value output returned by newuoa (list of length 4)
#' @param pre preprocessed object
#' @param data0 original data
#'
#' @return a list of post processing values
#' @export
postprocess <- function(data, model, newuoa_value, data0, pre){

  final_eta <- newuoa_value$par %>% magrittr::set_names(names(pre$par))

  if(is.nan(newuoa_value$fval)) {
    final_eta <- rep(0, length(diag(model$param_omega_matrix))) %>% magrittr::set_names(names(pre$par))
    warning("Cannot compute objective function value ; typical value (ETA = 0) returned")
  }

  carry <- data %>%
    select(-any_of(c("ID", "time","DV"))) %>%
    names()

  typical_pred <- model$mrgsolve_model %>%
    data_set(data) %>%
    zero_re() %>%
    mrgsim(carry_out = carry, end = -1) %>%
    as_tibble() %>%
    pull(.data$DV)

  mapbay_tab <- model$mrgsolve_model %>%
    param(final_eta) %>%
    data_set(data) %>%
    zero_re() %>%
    mrgsim(carry_out = carry, end = -1) %>%
    as_tibble() %>%
    mutate(IPRED = .data$DV, PRED = typical_pred, .after = "DV") %>%
    mutate(DV = data$DV) %>%
    mutate(evid = data0$evid) %>%
    select(-any_of(model$mrgsolve_model@cmtL))

  list(
    data = data,
    model = model,
    initial_eta  = pre$par,
    newuoa_value = newuoa_value,
    final_eta    = final_eta,
    mapbay_tab   = mapbay_tab
  )

}
