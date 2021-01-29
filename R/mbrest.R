#' Perform mapbayesian estimation from a mrgsolve model and a NM_tran dataset
#'
#' @param x model file
#' @param data NM tran data to optimize
#' @param method "newuoa" or "L-BFGS-B"
#' @param output return a mapbay_tab only
#' @param verbose a logical. If TRUE (the default), will print the steps of optimization.
#' @param control a list passed to the optimizer (see source code for default, as function of the optimizer)
#' @param force_initial_eta a numeric vector of starting estimates (exact length of eta to estimate )
#' @param quantile_bound for L-BFGS-B only: a numeric value of the probability expected as extreme value for a ETA
#' @param reset boolean. Reset to different initial eta if LBFGS do not move (default to F)
#'
#' @return default: a list with data, model, initial and final eta, mapbay_tab and rough optimization output
#' @export
#'
mbrest <- function(x,
                   data = NULL,
                   method = "L-BFGS-B",
                   output = NULL,
                   verbose = TRUE,
                   control = list(),
                   force_initial_eta = NULL,
                   quantile_bound = 0.001,
                   reset = T){
  ok <- check_mapbayr_model(x)
  if(!isTRUE(ok)) stop(c("\n", paste(ok, collapse = "\n")), call. = F)

  arg.optim <- preprocess.optim(method = method, model = x, control = control, force_initial_eta = force_initial_eta, quantile_bound = quantile_bound)

  if(is.null(data)){
    data <- x@args$data
  }

  idata <- preprocess.data(data)

  arg.ofv <- idata %>%
    map(preprocess.ofv, model = x)

  opt.value <- map(arg.ofv, do_optimization, arg.optim = arg.optim, verbose = verbose, reset = reset)

  post <- list(
    data = idata,
    opt.value = opt.value,
    arg.ofv = arg.ofv) %>%
    pmap(postprocess,
         model = x,
         arg.optim = arg.optim)

  output_mbr(idata = idata, model = x, arg.optim = arg.optim, arg.ofv = arg.ofv, opt.value = opt.value, post = post, output = output)
}


#' Check if model is valid for mapbayr
#'
#' @param x model file
#'
#' @return TRUE value if check is passed, a vector of character with errors otherwise.
#' @export
#'
#' @examples
#' library(mapbayr)
#' library(mrgsolve)
#' check_mapbayr_model(house())
check_mapbayr_model <- function(x){
 # browser()
  if(!is.mrgmod(x)){
    stop("the first argument to mbrest must be a model object", call. = F)
  }else{
    check <- character()
    if(!"DV" %in% x@capL) check <- c(check, "DV must be present in $CAPTURE")
    if(is.null(obs_cmt(x))) check <- c(check, "No [OBS] compartment(s) defined in $CMT")
    if(is.null(adm_cmt(x))) check <- c(check, "No [ADM] compartment(s) defined in $CMT")

    #@param has eta  ?
    #@param = as many as



  }
  if(length(check)==0) check <- TRUE
  return(check)
}
