mapbayr_plot <- function(aug_tab, obs_tab, PREDICTION = c("IPRED", "PRED")){

  validate_aug_tab(aug_tab)
  validate_obs_tab(obs_tab)

  predictions <- aug_tab %>%
    filter(.data$type %in% PREDICTION) %>%
    mutate(PREDICTION = .data$type)

  observations <- obs_tab %>%
    mutate(MDV = factor(.data$mdv, levels = c(0,1)))

  theme_custom <- function(...) {
    theme_bw(...) %+replace%
      theme(legend.position = "bottom",
            strip.background = element_rect(fill="white")
      )
  }

  gg <- predictions %>%
    ggplot(aes(.data$time, .data$value)) +
    geom_line(aes(col = .data$PREDICTION, linetype = .data$PREDICTION)) +
    theme_custom()+
    scale_color_manual(values= c(IPRED = "black", PRED = "deepskyblue1")) +
    scale_linetype_manual(values= c(IPRED = 1, PRED = 2))

  if(!is.null(predictions[["value_low"]]) & !is.null(predictions[["value_up"]])){
    data_ribbon <- predictions %>%
      filter(!(is.na(.data$value_low) & is.na(.data$value_up)))

    gg <- gg +
      geom_ribbon(aes(ymin = .data$value_low, ymax = .data$value_up, fill = .data$PREDICTION), data = data_ribbon, alpha = 0.3) +
      scale_fill_manual(values= c(IPRED = "black", PRED = "deepskyblue1"))
  }

  #MDV
  if(any(observations$mdv == 1)){
    gg <- gg+
      geom_point(data = observations, aes(y = .data$DV, shape = .data$MDV), fill = "black", size = 3)+
      scale_shape_manual(values= c(`0` = 21, `1` = 1))
  } else {
    gg <- gg+
      geom_point(data = observations, aes(y = .data$DV), fill = "black", size = 3, pch = 21)
  }

  #Facetting

  one_cmt <- (length(unique(predictions$name)) == 1) & (length(unique(observations$cmt[observations$evid%in%c(0,2)])) == 1)
  one_ID <- (length(unique(predictions$ID)) == 1) & (length(unique(observations$ID)) == 1)

  if(all(!one_cmt, !one_ID)) {
    gg <- gg+
      facet_grid(ID~cmt, scales = "free", labeller = label_both)
  }

  if(all(one_cmt, !one_ID)) {
    gg <- gg+
      facet_grid(ID~., scales = "free", labeller = label_both)
  }

  if(all(!one_cmt, one_ID)) {
    gg <- gg+
      facet_grid(.~cmt, scales = "free", labeller = label_both)
  }

  return(gg)
}

validate_aug_tab <- function(x){
  stopifnot(
    all(c("type", "ID", "time", "name", "value", "cmt") %in% names(x))
  )
}

validate_obs_tab <- function(x){
  stopifnot(
    all(c("ID", "time", "evid", "DV", "cmt", "mdv") %in% names(x))
  )
}
