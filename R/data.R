#' Generate Administration lines for a dataset
#'
#' @param model model object
#' @param time a numeric value
#' @param addl a numeric value
#' @param ii a numeric value
#' @param amt a numeric value
#' @param rate a numeric value (for IV only. Automatically filled with -2 if zero order set in model code)
#' @param realize_addl a logical (see mrgsolve::realize_addl)
#' @param output defaut : a mrgsolve model with a data_set args. if "df" return a data frame
#'
#' @return model object with dataset
#' @export
adm_lines <- function(model, time = 0, addl = 0, ii = 0, amt = 0, rate = 0, realize_addl = F, output = NULL){
  if(is.null(model@args$data)){
    model@args$data <- tibble()
    iID <- 1
  }else{
    iID <- utils::head(model@args$data$ID, 1)
  }

  d <- tibble(
    ID    = 1,
    time  = time,
    evid  = 1,
    addl  = addl,
    ii    = ii,
    amt   = amt,
    mdv   = 1
  ) %>%
    crossing(cmt = adm_cmt(model)) %>%
    mutate(rate = ifelse(.data$cmt %in% adm_0_cmt(model), -2, rate))

  if(realize_addl){
    d <- realize_addl(d)
  }


  d <- model@args$data %>%
    bind_rows(d) %>%
    arrange(.data$ID, .data$time, desc(.data$evid), .data$cmt)

  dd <- model %>%
    data_set(d)

  if(!is.null(output)){
    if(output == "df") dd <- dd@args$data
  }

  return(dd)
}



#' Generate observation lines for a dataset
#'
#' @param model model object
#' @param time vector of time
#' @param DV vector of values to fit
#' @param mdv should the Dv be ignored (1) or not (0)
#' @param DVmet optional : metabolite data to fit
#' @param output defaut : a mrgsolve model with a data_set args. if "df" return a data frame
#'
#' @return model object with dataset
#' @export
obs_lines <- function(model, time, DV, mdv = 0, DVmet = NULL, output = NULL){

  if(is.null(model@args$data)){
    model@args$data <- tibble()
    iID <- 1
  }else{
    iID <- utils::head(model@args$data$ID, 1)
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
    mutate(cmt = ifelse(.data$name == "DV", (obs_cmt(model))[1], (obs_cmt(model))[2])) %>%
    select(-any_of("name")) %>%
    mutate(ID = iID, evid = 0, addl = 0, ii = 0, amt = 0, rate = 0)



  d <- model@args$data %>%
    bind_rows(d) %>%
    arrange(.data$ID, .data$time, desc(.data$evid), .data$cmt)

  dd <- model %>%
    data_set(d)

  if(!is.null(output)){
    if(output == "df") dd <- dd@args$data
  }

  return(dd)

}



#' Add covariates to the dataset, as well as TOLA and AOLA
#'
#' @param model model object
#' @param covariates a list of named covariates, with a single value or exact number of lines than data
#' @param output defaut: a mrgsolve model with a data_set in args. if "df" return a data frame
#'
#' @return model object with dataset
#' @export
add_covariates <- function(model, covariates = list(), output = NULL){
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
      group_by(.data$ID) %>%
      mutate(TOLA = ifelse(.data$evid %in% c(1,4), .data$time, NA_real_)) %>%
      fill(.data$TOLA)%>%
      ungroup()
  }

  dd <- model %>%
    data_set(d)

  if(!is.null(output)){
    if(output == "df") dd <- dd@args$data
  }

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
