#' Title
#'
#' @param model a mapbay_model list, with the characteristics of the model, including a compiled mrgsolve_model
#' @param data a dataframe, dataset (NM-TRAN format) of one individual to fit
#' @param force_initial_eta a numeric vector of starting estimates (exact length of eta to estimate )
#'
#' @return a data.frame ready for mapbay_estimation
#' @export
preprocess <- function(model, data, force_initial_eta = NULL){


  n_omega <- length(diag(model$param_omega_matrix))

  if(is.null(force_initial_eta)){
    initial_eta <- runif(n_omega, -0.5, 0.5) %>% magrittr::set_names(str_c("ETA", 1:n_omega))
  } else {
    initial_eta <- force_initial_eta %>% magrittr::set_names(str_c("ETA", 1:n_omega))
  }

  if(nrow(data %>% filter(.data$time == 0, .data$mdv ==0)) > 0) stop("Observation line (mdv = 0) not accepted at t0 (time = 0)")

  data_to_fit <- data %>%
    filter(!(.data$evid%in%c(0,2)&.data$mdv==1))

  mrgsolve_model <- model$mrgsolve_model %>%
    #obsonly() %>%
    zero_re() %>%
    data_set(data_to_fit) %>%
    carry_out(cmt, evid)

  omega.inv <- solve(model$param_omega_matrix)

  DVobs <- data_to_fit[data_to_fit$evid%in%c(0,2),]$DV
  if(model$log.transformation){DVobs <- log(DVobs)}

 # f_compute_ofv <- ifelse(length(model$obs_cmt)>1,
 #                         compute_ofv_m,
 #                         compute_ofv)

  list(par = initial_eta,
       fn  = compute_ofv,
       data = data_to_fit,
       mrgsolve_model = mrgsolve_model,
       sigma = model$param_sigma_matrix,
       log.transformation = model$log.transformation,
       DVobs = DVobs,
       omega.inv = omega.inv,
       obs_cmt = model$obs_cmt,
       control = list(iprint = 2, maxfun = 50000)
       )
}
