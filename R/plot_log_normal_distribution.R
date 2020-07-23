

#' Title
#'
#' @param PAR character, name of the parameter
#' @param TV numeric, typical value of the paremeter
#' @param OM numeric, diagonal value of omega
#' @param EBE numeric, empirical bayesian estimate value
#' @param UNIT character, unit of the parameter
#'
#' @return a ggplot-object
#' @export
#' @importFrom ggplot2 ggplot aes stat_function geom_vline scale_y_continuous scale_x_continuous theme theme_grey %+replace% element_rect
#' @importFrom dplyr tibble
#' @importFrom stats dlnorm qlnorm
#' @importFrom rlang .data
#' @importFrom magrittr %>%
#'
plot_log_normal_distribution <- function(PAR, TV, OM, EBE, UNIT){

  theme_custom <- function(...) {
    theme_grey(...) %+replace%
      theme(
        panel.background = element_rect(fill = "white", color = "black")
      )
  }


  tibble(x = qlnorm(c(0.01, 0.99), log(TV), sqrt(OM))) %>%
    ggplot(aes(.data$x))+
    stat_function(fun = function(x){dlnorm(x, log(TV), sqrt(OM))}, geom = "density", fill = "skyblue", col = "skyblue", alpha =0.3)+
    geom_vline(xintercept = TV*exp(EBE), size = 1, linetype = 2)+
    scale_y_continuous(name = "",
                       breaks = NULL)+
    scale_x_continuous(name = PAR,
                       breaks = signif(TV*exp(EBE), 4),
                       labels = function(x){paste0(x, " ", UNIT)})+
    theme_custom()
}

