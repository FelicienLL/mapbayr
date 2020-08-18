#' Title
#'
#' @param eta a vector, initial eta, as a named vector (ETA1, ETA2...)
#' @param data a tibble, dataset to fit; formatted like NM-TRAN
#' @param mrgsolve_model a compiled mrgsolve model
#' @param sigma a matrix, population values of sigma. 1 proportional error, 2 additive error
#' @param log.transformation logical operator. Useful for proportional error models (i.e. log transformed additive)
#' @param DVobs vector of observation (provided by mapbay_estimation to avoid its calculation at each iteration)
#' @param omega.inv inverse of omega matix (provided by mapbay_estimation to avoid its calculation at each iteration)
#' @param obs_cmt vector of obs cmt (not used)
#'
#' @return a single value (the objective function value)
#' @export
#' @import mrgsolve
#' @importFrom magrittr %>%
#'
#'
#'
compute_ofv <- function(eta, data, mrgsolve_model, sigma, log.transformation, DVobs, omega.inv, obs_cmt){

  if(log.transformation){DVobs <- log(DVobs)}

  mrgsolve_model <- mrgsolve_model %>%
    param(as.list(eta))

  output <- mrgsolve_model %>%
    obsonly %>%
    zero_re() %>%
    data_set(data) %>%
    mrgsim

  DVpred <- output$DV

  if(log.transformation){DVpred <- log(DVpred)}

  H <- as.matrix(
    data.frame(
      H1 = DVpred,                #err prop
      H2 = 1                      #err add
    )
  )

  Sigsq <- diag(H %*% sigma %*% t(H))

  Deviation_concentration <- sum(log(Sigsq) + (DVobs - DVpred)^2/Sigsq)

  Deviation_parameter <- diag(matrix(eta, nrow = 1) %*% omega.inv %*% matrix(eta, ncol = 1))

  OFV <- Deviation_concentration + Deviation_parameter

  return(OFV) #value  : ofv

}
