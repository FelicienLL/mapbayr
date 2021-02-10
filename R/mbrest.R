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
#' @param reset logical. Reset to different initial eta if LBFGS do not move (default is TRUE)
#' @param check logical. Check model code for mapbayr specification (default is TRUE)
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
                   reset = T,
                   check = T){
  if(check){
    ok <- check_mapbayr_model(x)
    if(!isTRUE(ok)){
      if(any(ok$stop)) stop(paste(ok[ok$stop==T,]$descr, collapse = "\n"), call. = F)
    }
  }

  arg.optim <- preprocess.optim(method = method, model = x, control = control, force_initial_eta = force_initial_eta, quantile_bound = quantile_bound)

  if(is.null(data)){
    data <- x@args$data
  }

  idata <- preprocess.data(data)

  arg.ofv <-  map(idata, preprocess.ofv, model = x)

  opt.value <- map(arg.ofv, do_optimization, arg.optim = arg.optim, verbose = verbose, reset = reset)

  post <- list(
    data = idata,
    opt.value = opt.value,
    arg.ofv = arg.ofv) %>%
    pmap(postprocess,
         model = x,
         arg.optim = arg.optim)

  out <- output_mbr(idata = idata, model = x, arg.optim = arg.optim, arg.ofv = arg.ofv, opt.value = opt.value, post = post, output = output)

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
