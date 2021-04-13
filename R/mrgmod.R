#-------------------------
#---   Data helpers   ----
#-------------------------

#' Data helpers
#'
#' @name data_helpers
#' @param x model object
#' @param ... passed to `mrgsolve::ev()` in `adm_lines()`
#' @param time,DV,mdv,DVmet passed to `obs_lines()`
#' @param covariates a list of named covariates, with a single value or same number of lines than data
#'
#' @description Helpful functions to pass information about administrations (`adm_lines()`), observations (`obs_lines()`) and covariates (`add_covariates()`).
#' These functions are passed to a `mrgmod` object (mrgsolve model), and return a `mrgmod` object with a dataset inside, so that mrgsolve or mapbayr functions can be passed along within a pipe-friendly workflow.
NULL
#> NULL








#' @rdname data_helpers
#' @export
adm_lines <- function(x, ...) UseMethod("adm_lines")

#' Add administrations lines to data
#'
#' @param x model object
#' @param ... passed to mrgsolve::ev
#' @method adm_lines mrgmod
#' @return model object with dataset
#' @export
adm_lines.mrgmod <- function(x, ...){
  #if (!mrgsolve:::is.mrgmod(x))
  #  mrgsolve:::mod_first()

  if(is.null(x@args$data)){
    d0 <- tibble()
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
obs_lines <- function(x, time, DV, mdv = 0, DVmet = NULL, ...) UseMethod("obs_lines")

#' Add observations lines to data
#'
#' @param x model object
#' @param time vector of time
#' @param DV vector of values to fit
#' @param mdv should the Dv be ignored (1) or not (0)
#' @param DVmet optional : metabolite data to fit
#' @param ... not used
#' @method obs_lines mrgmod
#'
#' @return model object with dataset
#' @export
obs_lines.mrgmod <- function(x, time, DV, mdv = 0, DVmet = NULL, ...){

  if(is.null(x@args$data)){
    d0 <- tibble()
  } else {
    d0 <- x@args$data
  }

  if(is.null((d0[["ID"]]))){
    iID <- 1
  } else {
    iID <- (d0[["ID"]])[1]
  }

  d <- tibble(
    time = time,
    DV   = DV,
    mdv = mdv)

  if(!is.null(DVmet)){
    d <- d %>%
      mutate(DVmet = DVmet)
  }

  d <- d %>%
    pivot_longer(starts_with("DV"), values_to = "DV") %>%
    mutate(cmt = ifelse(.data$name == "DV", (obs_cmt(x))[1], (obs_cmt(x))[2])) %>%
    filter(!is.na(.data$cmt)) %>%
    select(-any_of("name")) %>%
    mutate(ID = iID, evid = 0, amt = 0)

  #Add these administrations to existing data and sort (adm (evid1) before obs (evid0) if same time = recsort 3/4)
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
add_covariates <- function(x, covariates, ...) UseMethod("add_covariates")

#' Add covariates columns to data
#' @param x model object
#' @param covariates a list of named covariates, with a single value or exact number of lines than data
#' @param ... not used
#' @method add_covariates mrgmod
#' @return model object with dataset
#' @export
add_covariates.mrgmod <- function(x, covariates = list(), ...){
  if(is.null(x@args$data)) stop("Please provide a dataset")

  d <- x@args$data %>%
    arrange(.data$ID, .data$time, -.data$evid, .data$cmt) %>%
    bind_cols(covariates)

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

  dd <- x %>%
    data_set(d)

  return(dd)

}


#' @rdname data_helpers
#' @export
see_data <- function(x, ...){
  warning("see_data() is deprecated. Use get_data() instead.")
  UseMethod("get_data")
}
