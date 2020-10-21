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

  if(is.null(model@args$data)){
    model@args$data <- tibble()
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
    mutate(ID = 1, evid = 0, addl = 0, ii = 0, amt = 0, rate = 0)

  if(is.null(model@args$data)){
    model@args$data <- tibble()
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

#' Print tibble data to the console
#'
#' @param x model object
#' @param ... passed to print
#'
#' @return called for its side effect
#' @export
see_data <- function(x, ...){
  print(as_tibble(x@args$data), ...)
}
