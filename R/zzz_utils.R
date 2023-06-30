#NAMESPACE ----------

#' @importFrom dplyr across all_of any_of arrange as_tibble bind_cols bind_rows case_when desc distinct everything filter full_join group_by
#' @importFrom dplyr left_join mutate pull relocate rename rename_with select slice starts_with summarise ungroup vars
#' @importFrom ggplot2 aes after_stat coord_cartesian element_rect facet_grid facet_wrap labeller geom_area geom_histogram geom_hline geom_line
#' @importFrom ggplot2 geom_point geom_ribbon geom_rug geom_segment geom_vline ggplot label_both labs theme_bw scale_x_continuous scale_y_continuous
#' @importFrom ggplot2 scale_y_log10 scale_fill_manual scale_shape_manual scale_color_manual scale_linetype_manual stat_function theme
#' @importFrom magrittr %>%
#' @importFrom mrgsolve as.list collapse_omega data_set ev is.mrgmod mread mcode mrgsim mrgsim_df mrgsim_q mvgauss omat outvars param realize_addl smat valid_data_set zero_re
#' @importFrom purrr flatten map map2 map2_dbl map2_dfc pmap pmap_dfr quietly safely simplify transpose
#' @importFrom rlang .data set_names
#' @importFrom stringr str_detect str_extract_all str_replace str_pad str_subset str_which
#' @importFrom tidyr expand_grid fill pivot_longer
NULL

#MISCELLANEOUS ----------

#' Get diagonal of omega matix
#'
#' @param x model object
#'
#' @return a vector of numeric
#' @noRd
odiag <- function(x){
  if(!is.mrgmod(x)) stop("the first argument to odiag must be a model object", call. = F)
  diag(omat(x, make = T))
}

#' Get quantile from omega diag and a probability
#'
#' @param x model object
#' @param .p percentile
#'
#' @return a vector of numeric
#' @noRd
get_quantile <- function(x, .p){
  if(!is.mrgmod(x)) stop("the first argument to lowbounds must be a model object", call. = F)
  sapply(sqrt(odiag(x)), stats::qnorm, p = .p, mean = 0)
}

my_percent <- function(x){
  stopifnot(is.numeric(x))
  paste0(round(x * 100, 0), "%")
}

eta_from_opt <- function(x){
  if(inherits(x, "minqa")){
    rename_as_eta(x$par)
  } else {
    x$par
  }
}

ci2q <- function(ci) (1-(ci/100))/2
znorm <- function(ci){
  stopifnot(is.numeric(ci), ci > 0, ci < 100)
  stats::qnorm(1-ci2q(ci))
}

namephicov <- function(n){
  unlist(map(seq_len(n), ~ paste0("ETC",.x,"_",unlist(combn(.x, 1, simplify = FALSE)))))
}

devalid_data_set <- function(x){
  as_tibble(x[,colnames(x)!="..zeros.."])
}

etanames_as_nonmem <- function(x){
  xl <- grepl("\\d{2,}", x)
  x[xl] <- gsub(pattern = "ETA", replacement = "ET", x = x[xl])
  x
}

