#' Compute the derivatives
#'
#' @param v_DV vector of concentrations to derivate (typically output$DV)
#' @param v_cmt vector of compartment associated to the concentrations (typically output$cmt)
#' @param cmts numbers of compartment compartment associated with an observations, to derive concentration (typically obs_cmt)
#'
#' @return a matrix
#' @export
#' @examples
#' derivatives(
#' v_DV = c(400, 40, 200, 20),
#' v_cmt = c(2, 3, 2, 3),
#' cmts = c(2,3)
#' )
#'
derivatives <- function(v_DV, v_cmt, cmts){
  LIST <- list()
  v_DV <- ifelse(v_DV == 0, 1, v_DV)
  for(i in cmts){
    condcmts <- v_cmt == i
    col_prop <- paste0("P", i)
    col_add <- paste0("A", i)
    LIST[[col_prop]] <- v_DV
    LIST[[col_prop]][!condcmts] <- 0
    LIST[[col_add]]  <- rep.int(1, length(v_DV))
    LIST[[col_add]][!condcmts]  <- 0
  }
  return(as.matrix(as.data.frame(LIST)))
}





#' Compute the objective function values. Called iteratively during minimization
#'
#' @param eta a vector, initial eta, as a named vector (ETA1, ETA2...)
#' @param mrgsolve_model a compiled mrgsolve model
#' @param data data set for administration and cmt/mdv data
#' @param sigma a matrix, population values of sigma. 1 proportional error, 2 additive error
#' @param log.transformation logical operator. Useful for proportional error models (i.e. log transformed additive)
#' @param DVobs vector of observation (provided by mapbay_estimation to avoid its calculation at each iteration)
#' @param omega.inv inverse of omega matix (provided by mapbay_estimation to avoid its calculation at each iteration)
#' @param obs_cmt vector of obs cmt (not used)
#'
#' @return a single value (the objective function value)
#' @export
compute_ofv <- function(eta, mrgsolve_model, data, sigma, log.transformation, DVobs, omega.inv, obs_cmt){

  #Update ETA values
  mod <- param(mrgsolve_model, as.list(eta))

  #Compute Predicted concentrations
  output <- mrgsim_q(x = mod, data = data, output = "df")

  #Add compartment column
  output[["cmt"]] <- data[["cmt"]]

  #Filter MDV == 0
  output <- output[data$mdv==0,]

  #Get DVpred
  DVpred <- output$DV
  if(log.transformation){DVpred <- log(DVpred)}

  #Compute H matrix
  H <- derivatives(v_DV = DVpred, v_cmt = output$cmt, cmts = obs_cmt)

  #Compute OFV
  Sigsq <- diag(H %*% sigma %*% t(H))
  sum(log(Sigsq) + (DVobs - DVpred)^2/Sigsq) + diag(matrix(eta, nrow = 1) %*% omega.inv %*% matrix(eta, ncol = 1))

}
