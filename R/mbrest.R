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
#' @param output if `NULL` (the default) a mbrests object is returned; if `df` a \emph{mapbay_tab} dataframe is returned
#'
#'
#'
#' @return a mbrests model object
#' @export
#'
mbrest <- function(x,
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
  if(check){
    ok <- check_mapbayr_model(x)
    if(!isTRUE(ok)){
      if(any(ok$stop)) stop(paste(ok[ok$stop==T,]$descr, collapse = "\n"), call. = F)
    }
  }

  arg.optim <- preprocess.optim(x, method = method, control = control, force_initial_eta = force_initial_eta, quantile_bound = quantile_bound)

  if(is.null(data)){
    data <- x@args$data
  }

  idata <- preprocess.data(data)

  arg.ofv <-  map(idata, preprocess.ofv, x = x)

  opt.value <- map(arg.ofv, do_optimization, arg.optim = arg.optim, verbose = verbose, reset = reset)

  post <- list(
    data = idata,
    opt.value = opt.value,
    arg.ofv = arg.ofv) %>%
    pmap(postprocess,
         x = x,
         arg.optim = arg.optim)

  out <- output_mbr(x, data = idata, arg.optim = arg.optim, arg.ofv = arg.ofv, opt.value = opt.value, post = post, output = output)

  return(out)
}

