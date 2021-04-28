#' Estimate parameters (maximum a posteriori)
#'
#'
#' @description
#' The main function of the mapbayr package. Performs a \emph{maximum a posteriori} Bayesian estimation of parameters, from a mrgsolve model object and a dataset containing information about administrations and observed concentrations.
#'
#' @param x the model object
#' @param data NMTRAN-like data set
#' @param method optimization method; possible values are `L-BFGS-B` (the default) and `newuoa`
#' @param force_initial_eta a vector of numeric values to start the estimation from (default to 0 for "L-BFGS-B")
#' @param quantile_bound a numeric value representing the quantile of the normal distribution admitted to define the bounds for L-BFGS-B (default is 0.001, i.e. 0.1%)
#' @param control a list passed to the optimizer (see \code{\link{optimx}} documentation)
#' @param check check model code for mapbayr specification (a logical, default is `TRUE`)
#' @param verbose print the steps of the estimations to the console (a logical, default is `TRUE`)
#' @param reset reset to different initial eta values if L-BFGS-B converges at initial values (a logical, default is `TRUE`)
#' @param output if `NULL` (the default) a mapbayests object is returned; if `df` a \emph{mapbay_tab} dataframe is returned
#'
#'
#'
#' @return a mapbayests model object
#' @export
#'
mapbayest <- function(x,
                   data = NULL,
                   method = "L-BFGS-B",
                   force_initial_eta = NULL,
                   quantile_bound = 0.001,
                   control = list(),
                   check = TRUE,
                   verbose = TRUE,
                   reset = TRUE,
                   output = NULL
){
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

  arg.optim   <- preprocess.optim(x, method = method, control = control, force_initial_eta = force_initial_eta, quantile_bound = quantile_bound)
  arg.ofv.fix <- preprocess.ofv.fix(x, data)

  iddata <- split_mapbayr_data(data)
  arg.ofv.id  <- map(iddata, preprocess.ofv.id, x = x)

  arg.ofv <- map(arg.ofv.id, ~ c(arg.ofv.fix, .x))

  opt.value <- map(arg.ofv, do_optimization, arg.optim = arg.optim, verbose = verbose, reset = reset)

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
                            output = output)

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
