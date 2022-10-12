#' Data helpers
#'
#' @name data_helpers
#' @description Create or modify a dataset from scratch, from a pre-existing dataset, or from a dataset stored into a 'mrgsolve' model
#'
#' @param x either a data.frame or a 'mrgsolve' model object
#' @param ID subject ID (default is 1)
#' @param time event time
#' @param evid event identification (default is 1 for administration, 0 for observation)
#' @param cmt compartment (no default, except if one `[ADM]` and `[OBS]` were tagged in the `$CMT` block in model code. See `details`.)
#' @param amt dose amount (for administration records only)
#' @param DV dependant value, i.e. observed concentration (for observation records only)
#' @param mdv missing dependant value (default is 0 for observation to take into account for parameter estimation, 1 otherwise)
#' @param addl additional dose (optional and for administration records only)
#' @param ss steady-state (optional and for administration records only. Is this dose the last of an infinity of administration? Yes, 1, or no, 0)
#' @param ii interdose interval (optional and for administration records only. Use it with `ss` and `addl`)
#' @param rate rate of administration (optional and for administration records only. Set to -2 if you model zero-order infusion. See `details`.)
#' @param ... additional columns or arguments for [mrgsolve::ev()]
#' @param DVmet passed to `obs_lines()`
#' @param covariates a list of named covariates, with a single value or same number of lines than data
#' @return a `mrgmod` object, with a dataset in the `@args$data` slot.
#'
#'
#' @details
#' Helpful functions build the data set. Instead of painfully build a data set and mind how to format the data, you can pass information about :
#'
#' - administrations with `adm_lines()`,
#' - observations with `obs_lines()`
#' - covariates with `add_covariates()`.
#'
#' These functions are passed to a `mrgmod` object (mrgsolve model), and return a `mrgmod` object with a data set inside with the correct formatting (so-called NM-TRAN format), so that mrgsolve or mapbayr functions can be passed along within a pipe-friendly workflow.
#'
#' These functions are meant to be used for one single patient at a time. Multiple ID is accepted, but the user is asked to check if the output is acceptable.
#'
#' @examples
#' library(magrittr)
#' # First, import a model
#' mod <- exmodel(add_exdata = FALSE)
#'
#' mod %>%
#'   adm_lines(amt = 10000, cmt = 1) %>%
#'   obs_lines(time = c(1.5, 4.4, 7.5, 24.6), DV = c(91.2904, 110.826, 79.384, 20.6671), cmt = 2) %>%
#'   # get_data() # for curiosity, you can extract the data set at this step
#'   mapbayest()
#'
#' # If `[ADM]` or `[OBS]` are set in `$CMT` in model code, the `cmt =` argument are superfluous.
#'
NULL
#> NULL

#' @rdname data_helpers
#' @export
adm_lines <- function(x, ...) {
  if(missing(x)) {
    adm_lines.missing(...)
  } else {
    UseMethod("adm_lines")
  }
}


#' Add administrations lines to data
#' @rdname data_helpers
#'
#' @method adm_lines data.frame
#' @export
adm_lines.data.frame <- function(x,
                                 ID = NULL,
                                 time = NULL,
                                 evid = 1L,
                                 cmt,
                                 amt,
                                 mdv = 1L,
                                 addl = NULL,
                                 ss = NULL,
                                 ii = NULL,
                                 rate = NULL,
                                 ...){
  old_data <- x

  # ID
  if(is.null(ID)){
    cur_ID <- utils::tail(old_data[["ID"]], 1)
    if(is.null(cur_ID)){
      ID <- 1L
    } else {
      ID <- cur_ID
    }
  }

  # TIME
  if(is.null(time) && (all(old_data[["time"]]==0) | nrow(old_data)==0)){
    time <- 0
  }

  # MATCH time & amt, and CROSS WITH cmt and rate
  if(length(time) == length(amt)){
    nadm <- length(time)
  } else {
    amttime <- expand.grid(amt, time)
    nadm <- nrow(amttime)
    amt <- amttime[,1]
    time <- amttime[,2]
  }
  amt <- rep(amt, each = length(cmt))
  time <- rep(time, each = length(cmt))
  cmt <- rep(cmt, nadm)
  rate <- rep(rate, nadm)

  # Call `mrgsolve::ev()`
  ev_args <- list(ID = ID, time = time, evid = evid, cmt = cmt, amt = amt, mdv = mdv, addl = addl, ss = ss, ii = ii, rate = rate, ... = ...)
  ev_args <- NULL_remove(ev_args)

  new_lines <- as.data.frame(do.call(ev, ev_args))
  new_data <- bind_rows(old_data, new_lines)
  rearrange_nmdata(new_data)
}

#' Add administrations lines to data
#' @rdname data_helpers
#'
#' @method adm_lines missing
#' @export
adm_lines.missing <- function(...){
  x <- as_tibble(data.frame())
  adm_lines.data.frame(x, ...)
}

#' Add administrations lines to data
#' @rdname data_helpers
#'
#' @method adm_lines mrgmod
#' @export
adm_lines.mrgmod <- function(x, cmt = adm_cmt(x), rate = NULL, ...){
  #x = a model object
  old_data <- get_data.mrgmod(x) #if no data: an empty 0x0 tibble, not "NULL"!

  if(is.null(rate)){
    zero_order_cmt <- adm_0_cmt(x)
    if(!is.null(zero_order_cmt) && any(cmt==zero_order_cmt)){
      rate <- rep(0, length(cmt))
      rate[cmt==zero_order_cmt] <- -2
    }
  }

  args <- list(x = old_data, cmt = cmt, rate = rate, ... = ...)
  args <- NULL_remove(args)

  new_data <- do.call(adm_lines.data.frame, args)

  data_set(x, new_data)
}







#' @rdname data_helpers
#' @export
obs_lines <- function(x, ...){
  if(missing(x)) {
    obs_lines.missing(...)
  } else {
    UseMethod("obs_lines")
  }
}



#' Add observations lines to data
#'
#' @method obs_lines data.frame
#' @rdname data_helpers
#' @export
obs_lines.data.frame <- function(x,
                                 ID = NULL,
                                 time,
                                 evid = 0L,
                                 cmt,
                                 DV = NA_real_,
                                 mdv = NULL,
                                 ...){

  old_data <- x

  # ID
  if(is.null(ID)){
    cur_ID <- utils::tail(old_data[["ID"]], 1)
    if(is.null(cur_ID)){
      ID <- 1L
    } else {
      ID <- cur_ID
    }
  }

  # Match time and cmt to the length of DV (especially if DV is parent + metab)
  cmttime <- expand.grid(cmt, time)
  #nobs <- nrow(cmttime)
  cmt <- cmttime[,1]
  time <- cmttime[,2]

  #MDV
  if(is.null(mdv)){
    mdv <- as.integer(is.na(DV))
  }

  new_lines <- data.frame(ID = ID, time = time, evid = evid, cmt = cmt, DV = DV, mdv = mdv, ... = ...)

  new_data <- bind_rows(old_data, new_lines)
  rearrange_nmdata(new_data)
}

#' Add observations lines to data
#'
#' @method obs_lines missing
#' @rdname data_helpers
#' @export
obs_lines.missing <- function(...){
  x <- as_tibble(data.frame())
  obs_lines.data.frame(x, ...)
}

#' Add observations lines to data
#'
#' @method obs_lines mrgmod
#' @rdname data_helpers
#' @export
obs_lines.mrgmod <- function(x, cmt = NULL, DV = NA_real_, DVmet = NULL, ...){
  #x = a model object
  old_data <- get_data.mrgmod(x) #if no data: an empty 0x0 tibble, not "NULL"!

  #CMT
  model_obs_cmt <- obs_cmt(x)
  if(is.null(cmt)){
    if(is.null(DVmet)){
      cmt <- model_obs_cmt[1]
    } else {
      cmt <- model_obs_cmt
    }
  }

  #DVmet to DV
  if(!is.null(DVmet)){
    if(length(cmt)!=2) stop("Define 2 observation compartments with the [OBS] tags in model code")
    nobs <- length(DV)
    if(nobs != length(DVmet)) stop("`DVmet` must be the same length as `DV`")
    DV <- as.double(rbind(DV, DVmet)) # = pivot longer: DV[1] DVmet[1] DV[2] DVmet[2] etc
    DVmet <- NULL
  }

  args <- list(x = old_data, cmt = cmt, DV = DV, ... = ...)
  args <- NULL_remove(args)

  new_data <- do.call(obs_lines.data.frame, args)

  data_set(x, new_data)
}

#' @rdname data_helpers
#' @export
add_covariates <- function(x, ...) UseMethod("add_covariates")

#' Add covariates columns to data
#' @method add_covariates mrgmod
#' @rdname data_helpers
#' @export
add_covariates.mrgmod <- function(x, ..., covariates = list()){
  if(is.null(x@args$data)) stop("Please provide a dataset")

  d <- arrange(x@args$data, .data$ID, .data$time, -.data$evid, .data$cmt)

  if(length(covariates)!=0){
    d <- bind_cols(d, covariates)
  } else {
    dots <- list(...)
    if(length(dots) != 0){
      if((is.null(names(dots[1]))||names(dots[1])=="") & is.list(dots[[1]]) & !is.null(names(dots[[1]]))){
        warning("A list was passed as first argument to `add_covariates()`, thus will be interpretated as a list of covariates. This behaviour will be deprecated. Please modify and use the argument add_covariates(covariates = ) explicitely.")
        d <- bind_cols(d, dots[[1]])
      } else {
        if(any(is.null(names(dots)))){
          stop("Arguments must be named (with covariates names)")
        }
        d <- bind_cols(d, dots)
      }
    }
  }

  if("AOLA" %in% mbr_cov_names(x)) {
    d <- d %>%
      group_by(.data$ID) %>%
      mutate(AOLA = ifelse(.data$evid %in% c(1,4), .data$amt, NA_real_)) %>%
      fill(.data$AOLA) %>%
      ungroup()
  }

  if("TOLA" %in% mbr_cov_names(x)) {
    d <- d %>%
      realize_addl() %>%
      arrange(.data$ID, .data$time, -.data$evid, .data$cmt) %>%
      group_by(.data$ID) %>%
      mutate(TOLA = ifelse(.data$evid %in% c(1,4), .data$time, NA_real_)) %>%
      fill(.data$TOLA)%>%
      ungroup()
  }

  dd <- data_set(x, d)

  return(dd)

}

NULL_remove <- function(x){
  x[!sapply(x,is.null)]
}

rearrange_nmdata <- function(x){
  #Sort ADM (evid1) before OBS (evid0) if same time = recsort 3/4
  x <- arrange(x, .data$ID, .data$time, desc(.data$evid), .data$cmt)

  # If no pre-existing AMT, RATE, SS, II or ADDL in former data, lines will be filled with NA -> fill with 0 instead
  if(!is.null(x[["amt"]]))   x$amt[is.na(x$amt)]   <- 0
  if(!is.null(x[["addl"]]))  x$addl[is.na(x$addl)] <- 0
  if(!is.null(x[["ii"]]))    x$ii[is.na(x$ii)]     <- 0
  if(!is.null(x[["rate"]]))  x$rate[is.na(x$rate)] <- 0
  if(!is.null(x[["ss"]]))    x$ss[is.na(x$ss)]     <- 0
  x
}
