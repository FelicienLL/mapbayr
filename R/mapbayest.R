#' Estimate parameters (maximum a posteriori)
#'
#'
#' @description
#' The main function of the mapbayr package. Performs a \emph{maximum a posteriori} Bayesian estimation of parameters, from a mrgsolve model object and a dataset containing information about administrations and observed concentrations.
#'
#' @param x the model object
#' @param data NMTRAN-like data set
#' @param method optimization method; possible values are `L-BFGS-B` (the default) and `newuoa`
#' @param hessian compute the hessian and return the variance-covariance matrix of estimation of etas (a logical, default is `TRUE`)
#' @param force_initial_eta a vector of numeric values to start the estimation from (default to 0 for "L-BFGS-B")
#' @param quantile_bound a numeric value representing the quantile of the normal distribution admitted to define the bounds for L-BFGS-B (default is 0.001, i.e. 0.1%)
#' @param control a list passed to the optimizer (see \code{\link{optimx}} documentation)
#' @param check check model code for mapbayr specification (a logical, default is `TRUE`)
#' @param verbose print the steps of the estimations to the console (a logical, default is `TRUE`)
#' @param reset reset optimizer with new initial eta values if numerical difficulties, or with new bounds (L-BFGS-B) if estimate equal to a bound. (a logical, default is `TRUE`)
#' @param output if `NULL` (the default) a mapbayests object is returned; if `df` a \emph{mapbay_tab} dataframe is returned
#'
#' @return a mapbayests object. Basically a list containing:
#'  - model: the model object
#'  - data: the analyzed dataset (might slightly differ from the one you passed, with an `mdv` column and some lower-case names).
#'  - arg.ofv.optim, arg.ofv.fix, arg.ofv.id: arguments passed to the optimization function. Useful for debugging but not relevant for a basic usage.
#'  - opt.value: the original output of the optimization function
#'  - final_eta: a list of individual vectors of final estimates. Access it with `x$final_eta` or `get_eta(x)`.
#'  - covariance: a list of individual variance-covariance matrix of estimation. Access it with `x$covariance` or `get_cov(x)`.
#'  - mapbay_tab: an output table containing the results of your estimations (data, IPRED, PRED, covariates, captured items, ETA etc...). Access it with `x$mapbay_tab`, `as.data.frame(x)` or `as_tibble(x)`.
#'  - information: run times and package versions.
#' @export
#' @examples
#' # First, code a model
#' code1 <- "$PARAM ETA1 = 0, ETA2 = 0,
#' KA = 0.5, TVCL = 1.1, TVV = 23.3
#' $OMEGA 0.41 0.32
#' $SIGMA 0.04 0
#' $CMT DEPOT CENT
#' $PK
#' double CL=TVCL*exp(ETA1+ETA(1));
#' double V=TVV*exp(ETA2+ETA(2)) ;
#' $ERROR
#' double DV=CENT/V*(1+EPS(1))+EPS(2);
#' $PKMODEL ncmt = 1, depot = TRUE
#' $CAPTURE DV CL
#' "
#'
#' my_model <- mrgsolve::mcode("my_model", code1)
#' # Then, import your data
#' my_data <- data.frame(ID = 1, TIME = c(0, 1.1, 5.2, 12.3), EVID = c(1,0,0,0), AMT = c(500, 0,0,0),
#'  CMT = c(1,2,2,2), DV = c(0, 15.1, 29.5, 22.3))
#' print(my_data)
#'
#' # And estimate
#' my_est <- mapbayest(x = my_model, data = my_data)
#' print(my_est)
#' # see also plot(my_est) and hist(my_est)
#'
#' # Use your estimation
#' get_eta(my_est)
#' get_param(my_est)
#' as.data.frame(my_est)
#'
mapbayest <- function(x,
                   data = NULL,
                   method = "L-BFGS-B",
                   hessian = TRUE,
                   force_initial_eta = NULL,
                   quantile_bound = 0.001,
                   control = list(),
                   check = TRUE,
                   verbose = TRUE,
                   reset = TRUE,
                   output = NULL
){

  # Start checks and pre-processing (i.e. generating arguments passed to the optimizer)
  t1 <- Sys.time()

  if(is.null(data)){
    data <- x@args$data
  }
  if(check){
    ok <- check_mapbayr_model(x)
    if(!isTRUE(ok)){
      if(any(ok$stop)) stop(paste(ok[ok$stop==T,]$descr, collapse = "\n"), call. = F)
    }
    data <- check_mapbayr_data(data)

    check_mapbayr_modeldata(x, data)
  }

  arg.optim   <- preprocess.optim(x, method = method, control = control, hessian = hessian, force_initial_eta = force_initial_eta, quantile_bound = quantile_bound)
  arg.ofv.fix <- preprocess.ofv.fix(x, data)

  iddata <- split_mapbayr_data(data)
  arg.ofv.id  <- map(iddata, preprocess.ofv.id, x = x)
  arg.ofv <- map(arg.ofv.id, ~ c(arg.ofv.fix, .x))

  # End checks and pre-processing
  t2 <- Sys.time()

  # Start optimization
  opt.value <- map(arg.ofv, do_optimization, arg.optim = arg.optim, verbose = verbose, reset = reset)

  # End optimization
  t3 <- Sys.time()

  # Start post-processing (i.e. generating output files)
  post <- list(
    data = iddata,
    opt.value = opt.value
    ) %>%
    pmap(postprocess.optim,
         x = x)

  out <- postprocess.output(x,
                            arg.optim = arg.optim,
                            arg.ofv.fix = arg.ofv.fix,
                            arg.ofv.id = arg.ofv.id,
                            opt.value = opt.value,
                            post = post,
                            output = output,
                            times = c(t1, t2, t3)
                            )

  return(out)
}

#' Estimate parameters (maximum a posteriori)
#' @param ... passed to mapbayest
#' @export
#' @rdname mapbayest
mbrest <- function(...){
  .Deprecated("mapbayest")
  mapbayest(...)
}
