#' Title
#'
#' @param pred_data a dataframe, NM-TRAN like
#' @param input_data a dataframe, NM-TRAN like
#' @param model a mapbay_model list, with the characteristics of the model, including a compiled mrgsolve_model
#' @param from a numeric input
#' @param to  a numeric input
#' @param hline a numeric input, can be provided by interpretation_tki$concentration_target
#'
#' @return a ggplot-object
#' @export
#'
#' @importFrom ggplot2 ggplot aes geom_line geom_point geom_hline labs theme_light scale_x_continuous scale_color_manual facet_grid
#' @importFrom dplyr filter vars mutate
#' @importFrom tidyr pivot_longer
#' @importFrom rlang .data set_names
#' @importFrom magrittr %>%
#' @importFrom stringr str_c
#'
#'
plot_pred_mapbay <- function(pred_data, input_data, model, from = 0, to = max(pred_data$time), hline = NULL){

  theme_custom <- function(...) {
    theme_light(...) %+replace%
      theme(legend.position = "none")
  }

  lab_y        <- str_c("Concentration ", model[["drug"]], " (", model[["concentration_unit"]], ")")
  lab_x        <- ifelse(to > 24*7, "Temps (j)", "Temps (h)")
  scaling_time <- ifelse(to > 24*7, 24, 1)

  scaling      <- model[["scaling_conc_from_user_to_model"]]


  if(length(model$obs_cmt) == 1 ){
    observations <- input_data %>%
      filter(.data$evid==0) %>%
      mutate(CONC = .data$DV)

    data_to_plot <- pred_data %>%
      mutate(CONC = .data$DV)
  } else {
    data_to_plot <- pred_data %>%
      pivot_longer(c("PAR", "MET"), "MOLECULE", values_to = "CONC")
    observations <- input_data %>%
      filter(.data$evid==0) %>%
      mutate(MOLECULE = ifelse(.data$cmt==model$obs_cmt[1], "PAR", "MET")) %>%
      mutate(CONC = .data$DV)

  }

  plot01 <- data_to_plot %>%
    filter(.data$time >= from) %>%
    filter(.data$time <= to) %>%
    ggplot(aes(.data$time / scaling_time, .data$CONC * scaling)) +
    geom_line(col = "black") +
    geom_point(data = observations, aes(col = as.factor(.data$mdv)), size = 3)+
    geom_hline(yintercept = hline, linetype = 2)+
    labs(y = lab_y, x = lab_x)+
    theme_custom()+
    scale_color_manual(values= c("deepskyblue1","black") %>% set_names(c(0,1)))

  if(length(model$obs_cmt) != 1){
    plot01 <- plot01+
      facet_grid(vars(.data$MOLECULE))
  }

  return(plot01)






}

