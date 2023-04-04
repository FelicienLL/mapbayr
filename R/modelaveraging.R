get_LL <- function(x, LL = TRUE){
  opt <- x$opt.value
  ans <- matrix(opt$value, dimnames = list(opt$ID, NULL))
  if(LL){
    ans <- exp(-0.5 * ans)
  }
  ans
}

get_AIC <- function(x){
  OFV <- get_LL(x, LL = FALSE)
  k <- sum(grepl("ETA", names(x$opt.value)))
  exp(-0.5 * OFV - k)
}

#' Compute Weights from Several Estimations
#'
#' @description
#' Model Averaging consists in analyzing the same data with different models
#' and to average their predictions.
#' In order to perform weigthed means of clearance predictions, (or
#' concentrations, or any metric of interest), it is necessary to compute
#' the "weight" of each estimation.
#' It is informed by the likelihood of estimation.
#' Two weighting scheme are currently implemented, one based on the log-
#' likelihood ("LL", the default), the other on the Akaike criterion ("AIC").
#' The method was previously described by Uster et al
#' [(Clinical Pharmacology and Therapeutics, 2021)](https://ascpt.onlinelibrary.wiley.com/doi/full/10.1002/cpt.2065).
#'
#' @param ... estimation objects generated from [mapbayest()] to compute weight from
#' @param scheme scheme weight, either "LL" or "AIC"
#' @param estlist a list of estimation objects. Overrides `...`
#'
#' @return a matrix of numeric, the weight of the estimation. There is one row per subject ID, one column per model. Consequently, the sum of each row is 1. Named estimation objects (either in `...` or in `estlist`) will be used as column names in the output.
#' @export
#'
#' @examples
#' library(magrittr)
#' same_data_and_est <- function(x){
#'   x %>%
#'     adm_rows(ID = 2, time = 0, amt = 100, addl = 3, ii = 24) %>%
#'     obs_rows(ID = 2, time = 96, DV = 1) %>%
#'     adm_rows(ID = 9, time = 0, amt = 200, addl = 3, ii = 24) %>%
#'     obs_rows(ID = 9, time = 96, DV = 1) %>%
#'     mapbayest()
#'   }
#'
#' mod <- exmodel(1, add_exdata = FALSE)
#'
#' est1 <- mod %>%
#'   mrgsolve::param(TVCL = 2) %>%
#'   same_data_and_est()
#'
#' est2 <- mod %>%
#'   mrgsolve::param(TVCL = 10) %>%
#'   same_data_and_est()
#'
#' model_averaging(CL2 = est1, CL10 = est2)
#' model_averaging(estlist = list(A = est1, B = est2, est1))
#'
model_averaging <- function(..., scheme = c("LL", "AIC"), estlist = NULL){
  dots <- list(...)

  if(is.null(estlist)){
    estlist <- dots
  } else {
    if(length(dots) > 0){
      message("estlist not NULL, arguments passed to `...` will be ignored")
    }
  }

  if(!all(sapply(estlist, inherits, "mapbayests"))){
    add_msg <- NULL
    if(all(sapply(estlist[[1]], inherits, "mapbayests"))){
      add_msg <- "\n Did you forget to call explicitely `estlist = `?"
    }
    stop("All objects passed to `model_averaging() must be `mapbayests` class object", add_msg)
  }

  IDs <- lapply(estlist, function(x){x$opt.value$ID})

  if(length(unique(IDs)) != 1){
    stop("Subject IDs are not the same between estimation objects")
  }

  scheme <- scheme[1]

  scheme_fn <- switch(scheme,
                      LL = get_LL,
                      AIC = get_AIC)

  values <- do.call(
    cbind,
    lapply(estlist, scheme_fn) #no sapply to keep rownames
  )
  colnames(values) <- names(estlist)

  values / rowSums(values)
}
