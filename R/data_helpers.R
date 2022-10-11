#' Data helpers
#'
#' @name data_helpers
#' @param x model object
#' @param ... passed to `mrgsolve::ev()` in `adm_lines()`
#' @param time,DV,mdv,cmt,DVmet passed to `obs_lines()`
#' @param covariates a list of named covariates, with a single value or same number of lines than data
#' @return a `mrgmod` object, with a dataset in the `@args$data` slot.
#'
#' @description Helpers to build data set.
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
adm_lines <- function(x, ...) UseMethod("adm_lines")

#' Add administrations lines to data
#' @rdname data_helpers
#'
#' @method adm_lines mrgmod
#' @export
adm_lines.mrgmod <- function(x, ...){
  if(is.null(x@args$data)){
    d0 <- as_tibble(data.frame())
  } else {
    d0 <- x@args$data
  }

  if(is.null((list(...)[["ID"]]))){
    if(is.null((d0[["ID"]]))){
      iID <- 1
    } else {
      iID <- (d0[["ID"]])[1]
    }
  } else {
    iID <- list(...)[["ID"]]
  }


  #Adm info passed to ev()
  d <- ev(..., mdv = 1) %>%
    as_tibble() %>%
    mutate(ID = iID) %>%
    select(any_of(c('ID', "time", "evid", "mdv", "amt", "addl", "ss", "ii", "rate", "cmt"))) #drop other columns

  #CMT
  #If cmt not explicitly provided in ..., set it to adm_cmt from model cod
  if(is.null((list(...)[["cmt"]]))){
    if(is.null(adm_cmt(x))){ #No [ADM] set in the model
      stop("Define administration compartment (with adm_lines(cmt = ...)) or in model code with the [ADM] tag in $CMT")
    }
    d <- d %>%
      select(-.data$cmt) %>% #if not supplied in ..., cmt is set by ev() with cmt = 1
      expand_grid(cmt = adm_cmt(x))
  }

  #RATE
  if(is.null((list(...)[["rate"]]))){   #If rate is not explicitly provided in ...,
    if(!is.null(adm_0_cmt(x))){     #check if needed with adm_0_cmt
      d <- d %>%
        mutate(rate = ifelse(.data$cmt %in% adm_0_cmt(x), -2, 0))
    }
    #otherwise: no rate and I'm fine with it
  }

  #Add these administrations to existing data and sort (adm (evid1) before obs (evid0) if same time = recsort 3/4)
  d <- bind_rows(d0, d) %>%
    arrange(.data$ID, .data$time, desc(.data$evid), .data$cmt)

  # If no pre-existing RATE, SS, II or ADDL in former data, lines will be filled with NA -> fill with 0 instead
  d <- NA_filler(d)

  dd <- data_set(x, d)

  return(dd)

}

#' @rdname data_helpers
#' @export
obs_lines <- function(x, ...) UseMethod("obs_lines")

#' Add observations lines to data
#'
#' @method obs_lines mrgmod
#' @rdname data_helpers
#' @export
obs_lines.mrgmod <- function(x, time, DV, mdv = NULL, cmt = NULL, DVmet = NULL, ...){

  if(is.null(x@args$data)){
    d0 <- as_tibble(data.frame())
  } else {
    d0 <- x@args$data
  }

  if(is.null((d0[["ID"]]))){
    iID <- 1
  } else {
    iID <- (d0[["ID"]])[1]
  }

  d <- data.frame(
    time = time,
    DV   = as.double(DV)) %>%
    as_tibble()

  # What mdv ?
  .mdv <- mdv
  if(is.null(.mdv)) .mdv <- as.double(is.na(DV))
  d$mdv <- .mdv

  # What cmt ?
  .cmt <- cmt
  if(is.null(.cmt)) .cmt <- (obs_cmt(x))
  if(is.null(.cmt))
    stop("Define observation compartment (with obs_lines(cmt = ...)) or in model code with the [OBS] tag in $CMT")

  if(!is.null(DVmet)){
    if(length(.cmt)!=2)
      stop("Define 2 observation compartments with the [OBS] tags in model code")
    d <- d %>%
      mutate(DVmet = DVmet)
  }

  d <- d %>%
    pivot_longer(starts_with("DV"), values_to = "DV") %>%
    mutate(cmt = ifelse(.data$name == "DV", as.integer(.cmt[1]), as.integer(.cmt[2]))) %>%
    filter(!is.na(.data$cmt)) %>%
    select(-any_of("name")) %>%
    mutate(ID = iID, evid = 0, amt = 0)

  #Add these observations to existing data and sort (adm (evid1) before obs (evid0) if same time = recsort 3/4)
  d <- d0 %>%
    bind_rows(d) %>%
    arrange(.data$ID, .data$time, desc(.data$evid), .data$cmt)

  # If no pre-existing RATE, SS, II or ADDL in former data, lines will be filled with NA -> fill with 0 instead
  d <- NA_filler(d)

  dd <- data_set(x, d)

  return(dd)

}

NA_filler <- function(data){
  if(!is.null(data[["addl"]]))  data$addl <- ifelse(is.na(data$addl), 0, data$addl)
  if(!is.null(data[["ii"]]))    data$ii   <- ifelse(is.na(data$ii),   0, data$ii)
  if(!is.null(data[["rate"]]))  data$rate <- ifelse(is.na(data$rate), 0, data$rate)
  if(!is.null(data[["ss"]]))    data$ss   <- ifelse(is.na(data$ss),   0, data$ss)
  return(data)
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
