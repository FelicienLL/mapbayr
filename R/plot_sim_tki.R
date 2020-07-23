#' Title
#'
#' @param sim_data a dataframe, tibble with 5th 50th and 95th percentile of simulation
#' @param model a mapbay_model list, with the characteristics of the model, including a compiled mrgsolve_model
#' @param zoom zoom input (shiny app button)
#' @param interpretation_tki a list, characteristic of the drug, with therapeutic target
#'
#' @return a ggplot-object
#' @export
#' @importFrom ggplot2 ggplot geom_ribbon geom_line geom_hline labs theme theme_light %+replace% coord_cartesian
#' @importFrom rlang .data
#' @importFrom magrittr %>%
#' @importFrom stringr str_c
#'
#'
plot_sim_tki <- function(sim_data, model,interpretation_tki, zoom){
  theme_custom <- function(...) {
    theme_light(...) %+replace%
      theme(legend.position = "bottom")
  }

  lab_y = str_c("Concentration ", model$drug, " (", model$concentration_unit, ")")
  lab_x = "Temps (j)"

  scaling <- model$scaling_conc_from_user_to_model

  plot4 <- sim_data %>%
    ggplot(aes(.data$TIME/24, .data$p50 * scaling))+
    geom_ribbon(aes(ymin = .data$p05 * scaling,
                    ymax = .data$p95 * scaling,
                    fill = .data$DOSING), alpha = .5)+
    geom_line(aes(col = .data$DOSING), size = 2)+
    geom_hline(yintercept = interpretation_tki$concentration_target, linetype = 2)+
    labs(y = lab_y, x = lab_x)+
    theme_custom()

  if(zoom==1){
    plot4 <- plot4+
      coord_cartesian(xlim = c(15+c(-1,0)))
  }else{
    plot4 <- plot4
  }

  plot4

}
