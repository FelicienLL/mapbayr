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



#' Pre-process: arguments for optimization function
#'
#' @inheritParams mbrest
#' @return a list of named arguments passed to optimizer (i.e. arg.optim)
#' @export
preprocess.optim <- function(x, method, control, force_initial_eta, quantile_bound){
  #Checks argument

  #method
  okmethod <- c("newuoa", "L-BFGS-B")
  if(!method %in% okmethod) stop(paste("Accepted methods:", paste(okmethod, collapse = ", "), '.'))
  method <- method[1]

  #par
  initial_eta <- force_initial_eta
  if(is.null(initial_eta)){
    if(method == "newuoa"){
      set.seed(1)
      initial_eta <- runif(n_eta(x), -0.01, 0.01)
      names(initial_eta) <- eta_names(x)
    }
    if(method == "L-BFGS-B"){
      initial_eta <- rep(0, n_eta(x))
      names(initial_eta) <- eta_names(x)
    }

  }

  #fn = compute_ofv

  #control
  if(is.null(control$trace)){
    control <- c(control, list(trace = 0))
  }
  if(is.null(control$kkt)){
    control <- c(control, list(kkt = FALSE))
  }
  if(method == "L-BFGS-B"){
    if(is.null(control$fnscale))
      control <- c(control, list(fnscale = 0.001))
    if(is.null(control$lmm))
      control <- c(control, list(lmm = 7))
  }

  #lower, upper
  bound = Inf
  if(method == "L-BFGS-B"){
    bound <- map_dbl(sqrt(odiag(x)), qnorm, p = quantile_bound, mean = 0)
  }

  arg <- list(
    par = initial_eta,
    fn = compute_ofv,
    method = method,
    control = control,
    lower = bound,
    upper = -bound
  )

  return(arg)
}


#' Pre-process: data into a list of individual data
#'
#' @inheritParams mbrest
#' @return a named list of data set (n individuals)
#' @export
preprocess.data <- function(data){
  if(is.null(data)) stop("No data provided", call. = F)

  data <- data %>%
    rename_with(tolower, any_of(c("TIME", "AMT", "MDV", "CMT", "EVID", "II", "ADDL", "SS", "RATE")))

  if(is.null(data[["ID"]]))   stop('ID column is missing', call. = F)
  if(is.null(data[["time"]])) stop('time column is missing', call. = F)
  if(is.null(data[["evid"]])) stop('evid column is missing', call. = F)
  if(is.null(data[["cmt"]]))  stop('cmt column is missing', call. = F)
  if(is.null(data[["amt"]]))  stop('amt column is missing', call. = F)
  if(is.null(data[["DV"]]))   stop('DV column is missing', call. = F)
  if(is.null(data[["mdv"]])){
    data[["mdv"]] <- ifelse(data[["evid"]] %in% c(1,2,4), 1, 0)
  }
  if(is.null(data[["mdv"]]))   stop('mdv column is missing', call. = F) #Cannot happen obviously... but who knows

  if(nrow(filter(data, .data$mdv == 0 & .data$evid == 2)) > 0) stop("Lines with evid = 2 & mdv = 0 are not allowed", call. = F)
  if(nrow(filter(data, .data$mdv == 0 & .data$evid != 0)) > 0) stop("Lines with mdv = 0 must have evid = 0.", call. = F)
  if(nrow(filter(data, .data$time == 0, .data$mdv == 0)) > 0)  stop("Observation line (mdv = 0) not accepted at time = 0", call. = F)

  iID <- unique(data$ID)

  idata <- data %>%
    mutate(split_ID = factor(.data$ID, levels = iID)) %>%
    group_by(.data$split_ID) %>%
    group_split(.keep = FALSE) %>%
    set_names(iID)

  return(idata)
}



#' Pre-process: model and data for ofv computation
#' @inheritParams mbrest
#' @return a list of named arguments passed to optimizer (i.e. arg.ofv)
#' @export
preprocess.ofv <- function(x, data){

  #mrgsolve_model
  q_model <- zero_re(x)
  q_model@end <- -1 #Make sure no modif in the time grid
  q_model@cmtL <- character(0) # Do not return amounts in compartments in the output
  q_model@Icmt <- integer(0)
  q_model@Icap <- which(x@capL== "DV") # Only return DV among $captured items
  q_model@capL <- "DV"

  #DVobs
  DVobs <- data[data$mdv==0,]$DV #keep observations to fit only
  if(log.transformation(x)){DVobs <- log(DVobs)}

  list(
    mrgsolve_model = q_model, #this model is 'updated' with zero re, end and "Req(DV)"
    data = data,
    sigma = smat(x, make = T),
    log.transformation = log.transformation(x),
    DVobs = DVobs,
    omega.inv = solve(omat(x, make = T)),
    obs_cmt = fit_cmt(x, data)
  )
}





