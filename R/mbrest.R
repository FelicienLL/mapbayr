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
    stop("the first argument must be a model object", call. = F)
  }else{
    check <- tibble(stop = logical(0), descr = character(0))

    # $PARAM
    neta <- length(eta_names(x))
    if(neta == 0) {
      check <- bind_rows(check, list(stop = TRUE, descr = "$PARAM: No ETA (ETA1, ETA2...) defined."))
    } else {
      if(any(eta_names(x) != paste0("ETA", seq.int(length.out = neta)))) check <-  bind_rows(check, list(stop = TRUE, descr = paste0("$PARAM: ", neta, " ETA found, but not sequentially named ETA1.")))
      if(!all(x[eta_names(x)]==0)) check <- bind_rows(check, list(stop = TRUE, descr = "$PARAM: Initial value is not 0 for all ETA."))
      if(any(is.na(eta_descr(x)))) check <- bind_rows(check, list(stop = FALSE, descr = "$PARAM: Description missing for at least one ETA (optionnal)."))
    }

    # $CMT
    if(is.null(adm_cmt(x))) check <- bind_rows(check, list(stop = FALSE, descr = "$CMT: No [ADM] compartment(s) defined (optionnal)."))
    if(is.null(obs_cmt(x))) check <- bind_rows(check, list(stop = FALSE, descr = "$CMT: No [OBS] compartment(s) defined (optionnal)."))

    # $OMEGA as much as ETA ?
    nomega <- length(diag(omat(x, make = T)))
    if(nomega != neta) check <- bind_rows(check, list(stop = TRUE, descr = "$OMEGA: Length of omega matrix diagonal not equal to the number of ETA defined in $PARAM."))

    # $SIGMA
    nsig <- length(diag(smat(x, make = T)))
    if(nsig%%2 !=0) check <- bind_rows(check, list(stop = TRUE, descr = paste0("$SIGMA: A pair number of sigma values is expected (", nsig, " values found).")))
    if(is.null(obs_cmt(x))){
      if(nsig != 2) check <- bind_rows(check, list(stop = TRUE,  descr = "$SIGMA: Define only one pair of sigma values (prop + add errors) in $SIGMA if you do not use [OBS] in $CMT. (One observation compartment will be defined from MDV=0 lines in individual data"))
    } else {
      ncmt <- length(obs_cmt(x))
      if(ncmt != nsig/2) check <- bind_rows(check, list(stop = TRUE, descr = "$SIGMA: Define one pair of sigma values (prop + add errors) per [OBS] compartment(s) defined in $CMT."))
    }

    # $CAPTURE
    if(!"DV" %in% x@capL) check <- bind_rows(check, list(stop = TRUE,  descr = "$CAPTURE: DV must be captured."))
    if(any(!(c("PAR", "MET") %in% x@capL)) & nsig > 2) check <- bind_rows(check, list(stop = TRUE,  descr = "$CAPTURE PAR and MET must be captured if multiple types of DV are fitted (more than one pair of sigma provided in $SIGMA)"))

  }
  if(nrow(check)==0) check <- TRUE
  return(check)
}
