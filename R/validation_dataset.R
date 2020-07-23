#' Generate and add random values of covariates to a dataset (used in validation_dataset)
#'
#' @param data a dataframe, NM-TRAN like
#' @param model a mapbay_model list, with the characteristics of the model, including a compiled mrgsolve_model
#' @param covariates list of the values of covariates to test (i.e. list(WT = c(60,80)))
#' @param random logical. If T, will add an other set of patients with changing covariates (randomly)
#'
#' @importFrom rlang .data set_names
#' @importFrom magrittr %>%
#' @importFrom purrr map map2 map_dfr flatten
#' @importFrom tidyr last_col
#' @importFrom dplyr relocate group_by mutate pull
#'
#' @return a dataframe, NM-TRAN like
#' @export
#'
#'
add_random_covariates <- function(data, model, covariates, random = F){
  ll <- covariates %>%
    map2(.y = names(covariates),
         function(.x, .y){
           if(random){
             rand <- data %>%
               group_by(.data$time) %>%
               mutate(a = sample(.x, 1, F)) %>%
               pull(.data$a) %>%
               list() %>%
               set_names(.y) %>%
               list()
           } else{
             rand <- NULL
           }

           c(.x %>%
               map(function(.x){
                 list(rep(.x, nrow(data))) %>%
                   set_names(.y)
               }),
             rand
           )
         })

  ll2 <- ll %>%
    flatten %>%
    map(function(.x){
      N <- names(.x)
      A <- as.list(model$covariate_ref_values)
      A[[N]] <- unname(unlist(.x))
      return(A)
    })

  ll2 %>%
    map_dfr(add_covariates, data = data, model = model, .id = "COV") %>%
    relocate(.data$COV, .after = last_col()) %>%
    mutate(ID = 100000 * as.numeric(.data$COV) + .data$ID)
}


#' Create a validation dataset (comparison vs NONMEM)
#'
#' @param model a mapbay_model list, with the characteristics of the model, including a compiled mrgsolve_model
#' @param min_n_day_ttt numeric input
#' @param max_n_day_ttt numeric input
#' @param by_n_day_ttt  numeric input
#' @param dosing_ii  numeric input
#' @param amts  numeric input (vector)
#' @param change_dose a logical value
#' @param n_random_samples_1 numeric input
#' @param n_random_samples_2 numeric input
#' @param covariates list of the values of covariates to test (i.e. list(WT = c(60,80)))
#' @param random logical. If T, will add an other set of patients with changing covariates (randomly)
#' @param nrep numeric input (n replicates)
#'
#' @importFrom rlang .data
#' @importFrom magrittr %>%
#' @importFrom tibble tibble
#' @importFrom tidyr crossing unnest
#' @importFrom dplyr group_by filter rowwise mutate cur_group_id ungroup select all_of slice_max transmute bind_rows arrange
#'
#' @return a dataframe, NM-TRAN like
#' @export
#'
#'
validation_dataset <- function(model, min_n_day_ttt, max_n_day_ttt, by_n_day_ttt, dosing_ii,
                               amts, change_dose = T, n_random_samples_1, n_random_samples_2,
                               covariates = NULL, random = F, nrep = 1){

  adm <- tibble(time = dosing_ii*c(0:max_n_day_ttt)) %>%
    crossing(i_n_day_ttt = seq(min_n_day_ttt, max_n_day_ttt, by_n_day_ttt)) %>%
    group_by(.data$i_n_day_ttt) %>%
    filter(.data$time <= dosing_ii * .data$i_n_day_ttt) %>%
    crossing(amt0 = amts) %>%
    crossing(amt_change = 0:change_dose) %>%
    rowwise() %>%
    mutate(amt = ifelse(.data$amt_change==1, sample(amts, 1, T), .data$amt0)) %>%
    group_by(.data$amt_change, .data$amt0, .data$i_n_day_ttt) %>%
    mutate(ID = cur_group_id() * 10) %>%
    ungroup() %>%
    select(all_of(c("ID", "time", "amt")))

  obs0 <- adm %>%
    select(all_of(c("ID", "time"))) %>%
    group_by(.data$ID) %>%
    slice_max(.data$time) %>%
    mutate(time_obs_max = .data$time + dosing_ii) %>%
    ungroup()

  obs1 <- obs0 %>%
    rowwise() %>%
    mutate(SEQ = list(c(sample(0:.data$time_obs_max, n_random_samples_1), .data$time_obs_max))) %>%
    unnest(.data$SEQ)%>%
    transmute(ID = .data$ID + 1, time = .data$SEQ, evid = 0)

  obs2 <- obs0 %>%
    rowwise() %>%
    mutate(SEQ = list(c(sample(.data$time:.data$time_obs_max, n_random_samples_2), .data$time_obs_max))) %>%
    unnest(.data$SEQ) %>%
    transmute(ID = .data$ID + 2, time = .data$SEQ, evid = 0)

  obs3 <- obs0 %>%
    transmute(ID = .data$ID + 3, time = .data$time_obs_max, evid = 0)

  obs99 <- bind_rows(obs1, obs2, obs3) %>%
    crossing(cmt = model$obs_cmt, mdv = 1, DV = NA_real_)

  adm99 <- adm %>%
    crossing(sampling_strat = c(1,2,3)) %>%
    crossing(cmt = model$adm_cmt) %>%
    mutate(evid = 1, mdv = 1, addl = 0, ii = 0, ID = .data$ID + .data$sampling_strat, .keep = "unused")

  data0 <- bind_rows(obs99, adm99) %>%
    arrange(.data$ID, .data$time, -.data$evid, .data$cmt) %>%
    add_rate_column(model)

  if(!is.null(covariates)){
    data0 <- data0 %>%
      add_random_covariates(model, covariates = covariates, random = random)
  }

  data99 <- data0%>%
    lag_covariates(model$covariate_names)

  data <- data99  %>%
    crossing(REP = 1:nrep) %>%
    mutate(ID = 1000 *.data$REP + .data$ID) %>%
    arrange(.data$ID, .data$time, -.data$evid, .data$cmt)

  return(data)

}
