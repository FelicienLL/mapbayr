#NAMESPACE ----------

#' @importFrom dplyr all_of any_of arrange as_tibble bind_cols bind_rows desc distinct everything filter group_by group_split mutate pull relocate rename rename_with select slice_max slice_min starts_with ungroup vars
#' @importFrom ggplot2 %+replace% aes element_rect facet_grid geom_hline geom_line geom_point geom_vline ggplot label_both labs theme_bw scale_shape_manual scale_color_manual stat_function theme
#' @importFrom ggpubr ggarrange
#' @importFrom magrittr %>%
#' @importFrom mrgsolve as.list data_set ev is.mrgmod mread mcode mrgsim mrgsim_df mrgsim_q mvgauss obsaug omat param realize_addl smat zero_re
#' @importFrom optimx optimx
#' @importFrom purrr map map2 map_dbl map_dfr pmap pmap_dfr quietly simplify transpose
#' @importFrom rlang .data set_names
#' @importFrom stats runif dnorm qnorm
#' @importFrom stringr str_subset str_detect str_which str_replace
#' @importFrom tibble tibble rownames_to_column
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

#' Internal "mapbayr" model examples
#'
#' @export
#'
#' @examples
#' \dontrun{
#' mod <- mrgsolve::mread("ex_mbr1", mbrlib())
#' mod <- mrgsolve::mread("ex_mbr2", mbrlib())
#' mod <- mrgsolve::mread("ex_mbr3", mbrlib())
#' }
mbrlib <- function(){
  system.file("models", package = "mapbayr")
}


