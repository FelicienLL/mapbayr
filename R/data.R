#' Generate Administration lines for a dataset
#'
#' @param x model object
#' @param ... passed to mrgsolve::ev
#'
#' @return model object with dataset
#' @export
adm_lines <- function(x, ...){
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

  dd <- data_set(x, d)

  return(dd)

}



#' Generate observation lines for a dataset
#'
#' @param x model object
#' @param time vector of time
#' @param DV vector of values to fit
#' @param mdv should the Dv be ignored (1) or not (0)
#' @param DVmet optional : metabolite data to fit
#'
#' @return model object with dataset
#' @export
obs_lines <- function(x, time, DV, mdv = 0, DVmet = NULL){

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


  if(!is.null(d0[["addl"]]))  d <- mutate(d, addl = 0)
  if(!is.null(d0[["ii"]]))    d <- mutate(d, ii = 0)
  if(!is.null(d0[["rate"]]))  d <- mutate(d, rate = 0)
  if(!is.null(d0[["ss"]]))    d <- mutate(d, ss = 0)

  #Add these administrations to existing data and sort (adm (evid1) before obs (evid0) if same time = recsort 3/4)
  d <- d0 %>%
    bind_rows(d) %>%
    arrange(.data$ID, .data$time, desc(.data$evid), .data$cmt)

  dd <- data_set(x, d)

  return(dd)

}



#' Add covariates to the dataset, as well as TOLA and AOLA
#'
#' @param model model object
#' @param covariates a list of named covariates, with a single value or exact number of lines than data
#'
#' @return model object with dataset
#' @export
add_covariates <- function(model, covariates = list()){
  if(is.null(model@args$data)) stop("Please provide a dataset")

  d <- model@args$data %>%
    arrange(.data$ID, .data$time, -.data$evid, .data$cmt) %>%
    bind_cols(covariates)

  if("AOLA" %in% mbr_cov_names(model)) {
    d <- d %>%
      group_by(.data$ID) %>%
      mutate(AOLA = ifelse(.data$evid %in% c(1,4), .data$amt, NA_real_)) %>%
      fill(.data$AOLA) %>%
      ungroup()
  }

  if("TOLA" %in% mbr_cov_names(model)) {
    d <- d %>%
      realize_addl() %>%
      arrange(.data$ID, .data$time, -.data$evid, .data$cmt) %>%
      group_by(.data$ID) %>%
      mutate(TOLA = ifelse(.data$evid %in% c(1,4), .data$time, NA_real_)) %>%
      fill(.data$TOLA)%>%
      ungroup()
  }

  dd <- model %>%
    data_set(d)

  return(dd)

}








#' Return tibble data
#'
#' @param x model object
#'
#' @return a tibble
#' @export
see_data <- function(x){
  as_tibble(x@args$data)
}
