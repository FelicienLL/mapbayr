#' Merge (validated) data with infered ETA
#'
#' @details
#' At the estimation step, data must be "validated" according to [mrgsolve::valid_data_set()].
#' It is a matrix, with a "valid_data_set" class, and a last column called "..zeros.."
#' The current function binds a matrix of ETA to a "valid" data set, as efficiently as possible.
#' @param validdata a matrix of class "valid_data_set"
#' @param eta a named vector of eta values
#' @param k multiply "eta" by k = 0.5 because eta are taken into account twice (through $PARAM and $OMEGA/etasrc)
#'
#' @return a matrix of class "valid_data_set"
#' @noRd
#' @examples
#' val <- mrgsolve::valid_data_set(exdata(1), exmodel(1))
#' eta <- eta(runif(3))
#'
#' merged <- merge_validdata_eta(val, eta)
#' mrgsolve:::is.valid_data_set(merged)
#'
merge_validdata_eta <- function(validdata, eta, k = 0.5){
  eta <- unlist(eta) #is sometime passed as a list
  ncol_validdata <- ncol(validdata)
  nrow_validdata <- nrow(validdata)

  proper_data <- validdata[,-ncol_validdata, drop = FALSE]
  zeros <- validdata[,ncol_validdata, drop = FALSE]
  eta_matrix <- matrix(
    data = eta * k,
    nrow = nrow_validdata,
    ncol = length(eta),
    byrow = TRUE, #faster than data = rep(eta, each = nrow)
    dimnames = list(NULL,
                    names(eta)
    )
  )

  ans <- cbind(proper_data, eta_matrix, zeros)
  # cannot use mrgsolve::valid_data_set() because it needs a model object
  class(ans) <- c("valid_data_set", "matrix")
  ans
}

#' Compute the H matrix
#'
#' @description Partial derivative of predictions with respect to epsilon
#'
#' @param pred predictions (typically obtained from `f()`)
#' @param cmt compartments predictions belong to (typically the `cmt` column of the dataset)
#' @param all_cmt all possible compartments with observations as defined in the model in `$SIGMA`
#' @param ... for compatibility (not used)
#'
#' @return a matrix of dimensions `[length(pred), 2 * length(all_cmt)]`
#'
#' @noRd
#' @examples
#' mapbayr:::h(
#' pred = c(400, 40, 200, 20),
#' cmt = c(2, 3, 2, 3),
#' all_cmt = c(2, 3)
#' )
h <- function(pred, cmt, all_cmt, ...){
  pred[pred==0] <- 1 #Avoid division by zero for proportional error
  m <- matrix(nrow = length(pred), ncol = 0)
  for(i in all_cmt){
    add <- as.double(cmt == i)
    m <- cbind(m, pred*add, add)
  }
  return(unname(m))
}

f <- function(qmod, data){
  output <- mrgsim_q(x = qmod, data = data, output = "df", etasrc = "data")
  output$DV[data[,"mdv"]==0]
}

ofv_kang <- function(obs, pred, eta, var, omega_inv, lambda = 1){
  eta <- unlist(eta)
  ofv_lik <- sum(log(var) + (obs - pred)^2/var) # length = 1
  ofv_pri <- diag(matrix(eta, nrow = 1) %*% omega_inv %*% matrix(eta, ncol = 1)) # length = 1
  ofv_lik + lambda * ofv_pri
}

#' Compute the objective function value
#' @param eta a named vector/list of parameters
#' @param qmod,sigma,log_transformation,omega_inv,all_cmt,lambda generated by \code{\link{preprocess.ofv.fix}}
#' @param idvaliddata,idDV,idcmt generated by \code{\link{preprocess.ofv.id}}
#' @param idblq,idlloq optionally generated by \code{\link{preprocess.ofv.id}}
#' @param argofv above mentioned arguments as a list
#' @param ... for compatibility (not used)
#'
#' @details
#' This function is called iteratively by the optimization function. Arguments should not be passed directly, but generated by the pre-processing functions (see \code{\link{preprocess.ofv}}).
#'
#' @return a single numeric value (the objective function value)
#' @export
compute_ofv <- function(eta, qmod, sigma, omega_inv, all_cmt, log_transformation, lambda = 1, idvaliddata, idDV, idcmt, idblq = NULL, idlloq = NULL, ...){
  #Update ETA values
  idvaliddata <- merge_validdata_eta(validdata = idvaliddata, eta = eta, k = 0.5)

  #Predict concentrations
  pred <- tryCatch(f(qmod = qmod, data = idvaliddata), silent = TRUE, error = function(x)NA)
  if(any(is.na(pred))) return(1E10)
  if(log_transformation){
    abspred <- abs(pred)
    small <- abspred < sqrt(.Machine$double.eps)
    if(any(small)){
      pred[small] <- abspred[small]
    }
    pred <- log(pred)
  }

  #Compute variance associated to predictions
  H <- h(pred = pred, cmt = idcmt, all_cmt = all_cmt)
  g2 <- diag(H %*% sigma %*% t(H))

  # Deal with BLQ data
  if(any(idblq)){
    ofv_blq <- sum(-2 * stats::pnorm(idlloq[idblq], pred[idblq], sqrt(g2[idblq]), lower.tail = TRUE, log.p = TRUE))
    ofv_obs <- ofv_kang(obs = idDV[!idblq], pred = pred[!idblq], eta = eta, var = g2[!idblq], omega_inv = omega_inv, lambda = lambda)
    return(sum(ofv_blq, ofv_obs))
  }

  #Compute the objective function value per se
  ofv_kang(obs = idDV, pred = pred, eta = eta, var = g2, omega_inv = omega_inv, lambda = lambda)
}

#' @rdname compute_ofv
#' @export
do_compute_ofv <- function(eta, argofv, ...){
  dots <- list(...)
  args <- c(list(eta = eta), argofv, dots)
  do.call(compute_ofv, args)
}
