#' Check if model is valid for mapbayr
#'
#' @param x model file
#' @param check_compile check if model is compiled (used internally)
#'
#' @return TRUE value if check is passed, a vector of character with errors otherwise.
#' @export
#'
#' @examples
#' library(mapbayr)
#' library(mrgsolve)
#' check_mapbayr_model(house())
check_mapbayr_model <- function(x, check_compile = TRUE){
  # browser()
  if(!is.mrgmod(x)){
    stop("the first argument must be a model object", call. = F)
  }else{
    check <- tibble(stop = logical(0), descr = character(0))

    # Structure

    if(!is.list(x@param@data)){
      stop("mod@param@data is not a list")
    }

    if(check_compile){
      if(!x@shlib$compiled){
        stop('model object is not compiled')
      }
    }

    # $PARAM
    neta <- length(eta_names(x))
    if(neta == 0) {
      check <- bind_rows(check, list(stop = TRUE, descr = "$PARAM: No ETA (ETA1, ETA2...) defined."))
    } else {
      if(any(eta_names(x) != paste0("ETA", seq.int(length.out = neta)))) check <-  bind_rows(check, list(stop = TRUE, descr = paste0("$PARAM: ", neta, " ETA found, but not sequentially named ETA1.")))
      if(!all(x[eta_names(x)]==0)) check <- bind_rows(check, list(stop = TRUE, descr = "$PARAM: Initial value is not 0 for all ETA."))
    }

    # $CMT
    if(is.null(adm_cmt(x))) check <- bind_rows(check, list(stop = FALSE, descr = "$CMT: No [ADM] compartment(s) defined (optionnal)."))
    if(is.null(obs_cmt(x))) check <- bind_rows(check, list(stop = FALSE, descr = "$CMT: No [OBS] compartment(s) defined (optionnal)."))

    # $OMEGA as much as ETA ?
    nomega <- length(diag(omat(x, make = T)))
    if(nomega != neta) check <- bind_rows(check, list(stop = TRUE, descr = "$OMEGA: Length of omega matrix diagonal not equal to the number of ETA defined in $PARAM."))

    # OMEGA : no value = 0.
    omega0 <- which(odiag(x) == 0)
    if(length(omega0)) check <- bind_rows(check, list(stop = TRUE, descr = paste0("$OMEGA: ", paste0(omega0, collapse = "-"), " is (are) equal to 0. Cannot be equal to zero.")))

    # $SIGMA
    nsig <- length(diag(smat(x, make = T)))
    if(nsig%%2 !=0) check <- bind_rows(check, list(stop = TRUE, descr = paste0("$SIGMA: A pair number of sigma values is expected (", nsig, " values found).")))
    if(is.null(obs_cmt(x))){
      if(nsig != 2) check <- bind_rows(check, list(stop = TRUE,  descr = "$SIGMA: Define only one pair of sigma values (prop + add errors) in $SIGMA if you do not use [OBS] in $CMT. (One observation compartment will be defined from MDV=0 lines in individual data"))
    } else {
      ncmt <- length(obs_cmt(x))
      if(ncmt != nsig/2) check <- bind_rows(check, list(stop = TRUE, descr = "$SIGMA: Define one pair of sigma values (prop + add errors) per [OBS] compartment(s) defined in $CMT."))
    }

    dsig <- diag(smat(x, make = T))
    if(all(dsig == 0)) check <- bind_rows(check, list(stop = TRUE, descr = paste0("$SIGMA: All the values of SIGMA are equal to zero, which is not allowed.")))

    if(log_transformation(x)){
      if(any(which(dsig==0)%%2 == 0)) check <- bind_rows(check, list(stop = TRUE, descr = "$SIGMA: Exponential error found. Sigma values in position 2,4... cannot be equal to 0."))
      if(any(which(dsig!=0)%%2 != 0)) check <- bind_rows(check, list(stop = TRUE, descr = "$SIGMA: Exponential error found. Sigma values in position 1,3... must be equal to 0."))
    }

    # $CAPTURE
    if(!"DV" %in% x@capL) check <- bind_rows(check, list(stop = TRUE,  descr = "$CAPTURE: DV must be captured."))
    if(any(!(c("PAR", "MET") %in% x@capL)) & nsig > 2) check <- bind_rows(check, list(stop = TRUE,  descr = "$CAPTURE PAR and MET must be captured if multiple types of DV are fitted (more than one pair of sigma provided in $SIGMA)"))

  }
  if(nrow(check)==0) check <- TRUE
  return(check)
}

check_mapbayr_data <- function(data){
  # Is there any data?
  if(is.null(data)) stop("No data provided", call. = F)

  # Are all column numerics
  non_num <- names(data)[!map_lgl(data, is.numeric)]
  if(length(non_num)) stop(paste("Non-numeric column found:", paste(non_num, collapse = " ")), call. = F)

  # Are required items present?
  data <- data %>%
    rename_with(tolower, any_of(c("TIME", "AMT", "MDV", "CMT", "EVID", "II", "ADDL", "SS", "RATE")))

  required_nmtran_item <- c("ID", "time", "evid", "cmt", "amt", "DV")
  miss_item <- required_nmtran_item[!(required_nmtran_item %in% names(data))]
  if(length(miss_item)) stop(paste("Missing column:", paste(miss_item, collapse = " ")), call. = F)
  if(is.null(data[["mdv"]])){
    data[["mdv"]] <- ifelse(data[["evid"]] %in% c(1,2,4), 1, 0)
  }
  if(is.null(data[["mdv"]]))  stop('mdv column is missing', call. = F) #Cannot happen obviously... but who knows

  # Are MDV/EVID requirements respected?

  if(nrow(filter(data, .data$mdv == 0 & .data$evid == 2)) > 0) stop("Lines with evid = 2 & mdv = 0 are not allowed", call. = F)
  if(nrow(filter(data, .data$mdv == 0 & .data$evid != 0)) > 0) stop("Lines with mdv = 0 must have evid = 0.", call. = F)
  if(nrow(filter(data, .data$time == 0, .data$mdv == 0)) > 0)  stop("Observation line (mdv = 0) not accepted at time = 0", call. = F)

  return(data)
}



check_mapbayr_modeldata <- function(x, data){
  # --- Checks full data vs model

  varinmodel <- c(names(x@param), as.list(x)$cpp_variables$var)
  varinmodel <- varinmodel[!varinmodel %in% c("DV", mbr_cov_names(x))]
  varindata <- names(data)
  commonvar <- varindata[varindata %in% varinmodel]

  if(length(commonvar) > 0) stop("These variables cannot be set in both model and data: ", paste(commonvar, collapse = ", "), '.', call. = FALSE)


  cmt_data <- obs_cmt_data(data)
  cmt_model <- obs_cmt(x)
  if(is.null(cmt_model)){
    if(length(cmt_data)!=1) stop(paste0("ID =", data$ID[1], "; CMT =", paste(cmt_data, collapse = " "), "\nMore than one 'observation compartment' to detect from data. Consider editing model code with [OBS] in $CMT."), call. = F)
    if(any(!(cmt_data %in% x@Icmt))) stop(paste0("ID =", data$ID[1], "; CMT =", cmt_data, "\n Compartment number with observation in dataset does not exist in model."))
  } else {
    if(any(!cmt_data %in% cmt_model)) stop(paste0("ID =", data$ID[1], "; CMT =", cmt_data, "\n One or more compartment with observation (mdv=0) in data don't match those defined with [OBS] in $CMT."), call. = F)
  }


}


split_mapbayr_data <- function(data){
  # --- Data split by ID
  split(data, ~factor(ID, levels = unique(data$ID)))
}


#' Pre-process: arguments for optimization function
#'
#' @inheritParams mapbayest
#'
#' @return a list of named arguments passed to optimizer (i.e. arg.optim)
#' @export
preprocess.optim <- function(x, method, control, force_initial_eta, quantile_bound){
  #Checks argument

  #method
  okmethod <- c("newuoa", "L-BFGS-B")
  if(!method %in% okmethod) stop(paste("Accepted methods:", paste(okmethod, collapse = ", "), '.'))
  method <- method[1]

  if(method == "newuoa"){
    if(!requireNamespace("minqa", quietly = TRUE)) {
      stop(
        "Package \"minqa\" must be installed to use method = \"newuoa\" ",
        call. = FALSE
      )
    }
  }

  #par
  initial_eta <- force_initial_eta
  if(is.null(initial_eta)){
    if(method == "newuoa"){
      initial_eta <- rep_len(0.01, n_eta(x))
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
  if(is.null(control$maxit)){
    control <- c(control, list(maxit = 9999))
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
  bound = -Inf
  if(method == "L-BFGS-B"){
    bound <- get_quantile(x, .p = quantile_bound)
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


#' Preprocess model and data for ofv computation
#'
#' @name preprocess.ofv
#' @param x the model object
#' @param data,iddata NMTRAN-like data set. iddata is likely a dataset of one individual
#' @return A list of arguments used to compute the objective function value.
#'
#' The following arguments are fixed between individuals:
#'
#'  - `qmod`: model object, modified to simulate without random effects and with controlled outputs
#'  - `sigma`: a single matrix object
#'  - `log_transformation`: a logical, whether predictions need to be log-transformed for ofv computation
#'  - `omega_inv`: a single matrix object
#'  - `all_cmt`: a vector of compartment numbers where observations can be expected
#'
#' The following arguments differs between individuals:
#'
#'  - `idvaliddata`: a matrix, individual data set (with administrations and covariates), validated with \code{\link[mrgsolve]{valid_data_set}}
#'  - `idDV`: a vector of (possibly log-transformed) observations
#'  - `idcmt`: a vector of compartments where observations belong to
#'
#' @examples
#' mod <- exmodel(add_exdata = FALSE, compile = FALSE)
#' dat <- exdata(ID = c(1,4))
#'
#' preprocess.ofv.fix(x = mod, data = dat)
#' preprocess.ofv.id(x = mod, iddata = dat[dat$ID == 1,])
#' preprocess.ofv.id(x = mod, iddata = dat[dat$ID == 4,])
#'
#' @description Functions to generate arguments passed to \code{\link{compute_ofv}}. Arguments that are fixed between individuals are created once (`preprocess.ofv.fix`), while others are specific of each individual (`preprocess.ofv.id`).
NULL
#> NULL

#' Preprocess fix arguments for ofv computation
#' @rdname preprocess.ofv
#' @export
preprocess.ofv.fix <- function(x, data){
  qmod <- zero_re(x)
  qmod@end <- -1 #Make sure no modif in the time grid
  qmod@cmtL <- character(0) # Do not return amounts in compartments in the output
  qmod@Icmt <- integer(0)
  qmod@Icap <- which(x@capL== "DV") # Only return DV among $captured items
  qmod@capL <- "DV"

  list(
    qmod = qmod,
    sigma = smat(x, make = T),
    log_transformation = log_transformation(x),
    omega_inv = solve(omat(x, make = T)),
    all_cmt = fit_cmt(x, data) #on full data
  )
}

#' Preprocess individual arguments for ofv computation
#' @rdname preprocess.ofv
#' @export
preprocess.ofv.id <- function(x, iddata){
  # --- Checks id data vs model

  #eg : at least one obs per id

  # --- Generate preprocess

  idDV <- iddata$DV[iddata$mdv==0] #keep observations to fit only
  if(log_transformation(x)) idDV <- log(idDV)
  idcmt <- iddata$cmt[iddata$mdv==0]

  list(idvaliddata = mrgsolve::valid_data_set(iddata, x),
       idDV = idDV,
       idcmt = idcmt
  )
}
