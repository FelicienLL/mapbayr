#' Preprocess: arguments for optimization function
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


#' Preprocess: data into a list of individual data
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



#' Preprocess: model and data for ofv computation
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







#' Post process results from optimization
#'
#' @inheritParams mbrest
#' @param opt.value value returned by optimizer
#' @param arg.optim,arg.ofv argument passed to optimizer
#'
#' @return a list of post processing values
#' @export
postprocess <- function(x, data, opt.value, arg.optim, arg.ofv){

  final_eta <- opt.value[eta_names(x)] %>%
    as.double() %>%
    set_names(eta_names(x))

  if(!is.null(opt.value$fevals)){
    if(is.nan(opt.value$fevals)) {
      final_eta <- rep(0, n_eta(x)) %>% set_names(eta_names(x))
      warning("Cannot compute objective function value ; typical value (ETA = 0) returned")
    }
  }

  typical_pred <- x %>%
    data_set(data) %>%
    zero_re() %>%
    mrgsim(end = -1) %>%
    as_tibble() %>%
    pull(.data$DV)

  indiv_pred <- x %>%
    param(final_eta) %>%
    data_set(data) %>%
    zero_re() %>%
    mrgsim(end = -1) %>%
    as_tibble() %>%
    pull(.data$DV)

  mapbay_tab <- data %>%
    mutate(IPRED = indiv_pred, PRED = typical_pred, .after = "DV") %>%
    select(-any_of(x@cmtL)) %>%
    bind_cols(bind_rows(final_eta))

  list(
    final_eta = final_eta,
    mapbay_tab = mapbay_tab
  )

}


#' Build the output of mbrest function
#' @inheritParams mbrest
#' @inheritParams postprocess
#' @param post output of the post.process function
#'
#' @return a mbrests model object
#' @export
output_mbr <- function(x, data, arg.optim, arg.ofv, opt.value, post, output){

  if(!is.null(output)){
    if(output == "df") out <- map_dfr(post, "mapbay_tab")

  } else {
    out <- list(
      model = x,
      data = bind_rows(unname(data)),
      arg.optim = arg.optim,
      arg.ofv = arg.ofv,
      opt.value = map_dfr(opt.value, rownames_to_column, var = "method", .id = "ID"),
      final_eta = map(post, "final_eta"),
      mapbay_tab = map_dfr(post, "mapbay_tab")
    )

    class(out) <- "mbrests"

  }
  return(out)

}
