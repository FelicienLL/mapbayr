#' Title
#'
#' @param h0 character input, initial hour
#' @param ... character input, hours to calculate difference from h0
#' @param date character input, optionnal date
#'
#' @return vector of numeric
#' @export
#' @importFrom lubridate ymd_hm
#' @importFrom purrr map
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#'
#'
hour_to_time <- function(h0, ..., date = '1970/01/01'){

  H0 <- ymd_hm(paste(date, h0))

  list(...) %>%
    map(function(x)paste(date, x)) %>%
    map(ymd_hm) %>%
    map(function(x)as.double.difftime(x - H0, units = "hours")) %>%
    unlist()
}

#' Title
#'
#' @param data a dataframe, NM-TRAN like
#'
#' @return a numeric value
#' @export
#' @importFrom rlang .data
#' @importFrom magrittr %>%
#' @importFrom dplyr filter mutate slice_max slice pull
#'
time_last_dose <- function(data){
  data %>%
    filter(.data$evid == 1) %>%
    mutate(t = .data$time + .data$addl * .data$ii) %>%
    slice_max(.data$t, 1) %>%
    slice(1) %>%
    pull(.data$t)
}


#' Title
#'
#' @param data a dataframe, NM-TRAN like
#'
#' @return a numeric value
#' @export
#' @importFrom rlang .data
#' @importFrom magrittr %>%
#' @importFrom dplyr filter mutate slice_max slice pull lead
#'
#'
time_target <- function(data){
  ti <- data  %>%
    addl_to_linebyline() %>%
    filter(.data$evid %in% c(1,4)) %>%
    group_by(.data$cmt) %>%
    slice_max(.data$time, n = 2) %>%
    mutate(ti = .data$time - lead(.data$time)) %>%
    ungroup() %>%
    slice(1) %>%
    pull(ti)

  time_last_dose(data) + ti
}




#' Title
#'
#' @param model a mapbay_model list, with the characteristics of the model, including a compiled mrgsolve_model
#' @param time a numeric value
#' @param addl a numeric value
#' @param ii a numeric value
#' @param amt a numeric value
#'
#' @return a tibble with administration
#' @export
#' @importFrom dplyr tibble select
#' @importFrom rlang .data
#' @importFrom magrittr %>%
#'
adm_lines <- function(model, time = 0, addl = 0, ii = 0, amt){
  d <- tibble(
    ID    = 1,
    time  = time,
    evid  = 1,
    addl  = addl,
    ii    = ii,
    amt   = amt,
    cmt   = model$adm_cmt,
    mdv   = 1)

  d

}

#' Title
#'
#' @param data a dataframe, NM-TRAN like
#'
#' @return a tibble with administration
#' @importFrom rlang .data
#' @importFrom magrittr %>%
#' @importFrom dplyr filter group_by mutate ungroup select arrange
#' @importFrom tidyr separate_rows
#' @export
#'
addl_to_linebyline <- function(data){

  data %>%
    filter(.data$evid == 1) %>%
    group_by(.data$ID, .data$cmt, .data$time) %>%
    mutate(XX = str_c(.data$time+c(0:.data$addl)*.data$ii, collapse = "o")) %>%
    separate_rows(.data$XX, sep = "o") %>%
    ungroup() %>%
    mutate(time = as.double(.data$XX), addl = 0, ii = 0) %>%
    select(-.data$XX) %>%
    bind_rows(filter(data, .data$evid !=1)) %>%
    arrange(.data$ID, .data$time, -.data$evid, .data$cmt)
  }



#' Title
#'
#' @param model a mapbay_model list, with the characteristics of the model, including a compiled mrgsolve_model
#' @param time a numeric vector of time after the last dose
#' @param DV a numeric vector of observation
#' @param mdv a numeric vector of mdv to ignore observation
#'
#' @return a tibble with observations
#' @importFrom dplyr tibble
#' @export
#'
obs_lines <- function(model, time, DV, mdv = 0){

  if(length(model$obs_cmt) > 1){
    cmt_val <- model$obs_cmt[1:length(DV)]
  } else {
    cmt_val <- model$obs_cmt
  }

  tibble(
    ID    = 1,
    time  = time,
    DV    = DV / model$scaling_conc_from_user_to_model,
    evid  = 0,
    mdv   = mdv,
    cmt   = cmt_val
  )

}



#' Title
#'
#' @param observation_lines a tibble with observations
#' @param administration_lines  a tibble with administration
#'
#' @return a dataframe, NM-TRAN like
#' @export
#' @importFrom rlang .data
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate bind_rows arrange
#'
#'
build_input_dataset <- function(observation_lines, administration_lines){
  observation_lines %>%
    mutate(time = .data$time + time_last_dose(administration_lines)) %>%
    bind_rows(administration_lines) %>%
    arrange(.data$ID, .data$time, -.data$evid, .data$cmt)
}


#' Title
#'
#' @param data  a dataframe, NM-TRAN like
#' @param model  a mapbay_model list, with the characteristics of the model, including a compiled mrgsolve_model
#' @param covariates a list of named covariates, with a single value or exact number of lines than data
#'
#' @return a dataframe, NM-TRAN like
#' @export
#' @importFrom dplyr as_tibble mutate bind_cols filter group_by last
#' @importFrom tidyr fill
#' @importFrom rlang .data
#' @importFrom magrittr %>%
#'
add_covariates <- function(data, model, covariates = list()){
  d <- data %>%
    arrange(.data$ID, .data$time, -.data$evid, .data$cmt) %>%
    bind_cols(covariates)

  if("AOLA" %in% model$covariate_names) {
    d <- d %>%
      mutate(AOLA = .data$amt) %>%
      fill(.data$AOLA)
  }

  if("TOLA" %in% model$covariate_names) {
    d <- d %>%
      addl_to_linebyline() %>%
      mutate(TOLA = ifelse(.data$evid %in% c(1,4), .data$time, NA_real_)) %>%
      fill(.data$TOLA)
  }

  if("MATIN" %in% model$covariate_names & !is.null(covariates$"MATIN")) {
    m_obs <- filter(d, .data$evid != 1) %>%
      mutate(MATIN = NA_real_)

    d <- d %>%
      addl_to_linebyline() %>%
      filter(.data$evid == 1) %>%
      group_by(.data$cmt) %>%
      mutate(MATIN = ifelse((last(.data$time)- .data$time) %% 24 == 0, .data$MATIN, 1 - .data$MATIN )) %>%
      ungroup() %>%
      bind_rows(m_obs) %>%
      arrange(.data$ID, .data$time, -.data$evid, .data$cmt) %>%
      group_by(.data$ID) %>%
      fill(.data$MATIN, .direction = "downup") %>%
      ungroup()

  }

  return(d)

}


#' Title
#'
#' @param data a dataframe, NM-TRAN like
#' @param model a mapbay_model list, with the characteristics of the model, including a compiled mrgsolve_model
#' @param rate a numeric input, rate of the perfusion. Default value = -2 : duration of infusion expected in the mrgsolve model
#'
#' @return a dataframe, NM-TRAN like
#' @importFrom rlang .data
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate case_when
#' @importFrom stringr str_subset str_match
#' @export
#'
add_rate_column <- function(data, model, rate = -2){

  if(rate != -2){
    cmt_rate <- model$adm_cmt
  } else {
    cmt_rate <- model$mrgsolve_model@code %>%
      str_subset("D_") %>%
      str_match("(?<=D_)\\w+") %>%
      match(model$mrgsolve_model@cmtL)
  }

  data %>%
    mutate(rate = case_when(
      !.data$evid%in%c(1,4) ~ 0,
      .data$evid%in%c(1, 4)&.data$cmt%in%cmt_rate ~ rate,
      T ~ 0
    ))

}

#' Title
#'
#' @param amt a numeric input
#' @param start a numeric input
#' @param end a numeric input
#'
#' @return a list of numerics
#' @export
#'
rate_duration <- function(amt, start, end){
  dur <- hour_to_time(start, end)

  rate <- amt/dur

  list(duration = dur,
       rate = rate)

}


#' Title
#'
#' @param data a dataframe, NM-TRAN like
#' @param covariates a vector of characters, names of the covariates column to "lag"
#'
#' @importFrom rlang .data
#' @importFrom magrittr %>%
#' @importFrom dplyr filter group_by mutate across any_of lag arrange bind_rows ungroup
#' @importFrom tidyr fill
#'
#' @return a dataframe, NM-TRAN like
#' @export
#'
#'
lag_covariates <- function(data, covariates){
  bind_rows(
    data %>%
      filter(.data$evid != 0) %>%
      group_by(.data$ID, .data$cmt) %>%
      mutate(across(any_of(covariates), lag, .names = "LAG_{col}")) %>%
      ungroup() %>%
      group_by(.data$ID) %>%
      fill(paste0("LAG_",covariates), .direction = "updown") %>%
      ungroup()
    ,
    data %>%
      filter(.data$evid == 0) %>%
      group_by(.data$ID, .data$cmt) %>%
      mutate(across(any_of(covariates), .names = "LAG_{col}")) %>%
      ungroup()
  ) %>%
    arrange(.data$ID, .data$time, -.data$evid, .data$cmt)
}

