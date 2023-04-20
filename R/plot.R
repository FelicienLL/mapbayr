mapbayr_plot <- function(aug_tab, obs_tab, PREDICTION = c("IPRED", "PRED")){

  validate_aug_tab(aug_tab)

  predictions <- aug_tab %>%
    filter(.data$type %in% PREDICTION) %>%
    mutate(PREDICTION = .data$type)

  if(all(unique(predictions$name) %in% c("PAR", "MET"))){
    predictions$name <- factor(predictions$name, c("PAR", "MET"))
  }

  # if(){
  #   predictions$MODEL <- "Model"
  # }

  theme_custom <- function(...) {
    theme_bw(...) %+replace%
      theme(legend.position = "bottom",
            strip.background = element_rect(fill="white")
      )
  }

  nmodels <- length(unique(predictions$MODEL))

  if(is.null(predictions[["MODEL"]])){
    aes_lines <- aes(
      col = .data$PREDICTION,
      linetype = .data$PREDICTION
    )

    aes_ribbon <- aes(
      ymin = .data$value_low,
      ymax = .data$value_up,
      fill = .data$PREDICTION
      )

    coloration_values <- c(IPRED = "black", PRED = "deepskyblue1")
  } else {
    aes_lines <- aes(
      col = .data$MODEL,
      linetype = .data$PREDICTION
      )

    aes_ribbon <- aes(
      ymin = .data$value_low,
      ymax = .data$value_up,
      fill = .data$MODEL
    )

    coloration_values <- scales::hue_pal()(nmodels)
  }

  gg <- predictions %>%
    ggplot(aes(.data$time, .data$value)) +
    geom_line(aes_lines) +
    theme_custom()+
    scale_color_manual(values = coloration_values) +
    scale_linetype_manual(values = c(IPRED = 1, PRED = 2))

  if(!is.null(predictions[["value_low"]]) & !is.null(predictions[["value_up"]])){
    data_ribbon <- predictions %>%
      filter(!(is.na(.data$value_low) & is.na(.data$value_up)))

    gg <- gg +
      geom_ribbon(aes_ribbon, data = data_ribbon, alpha = 0.3) +
      scale_fill_manual(values = coloration_values)
  }

  # Observations

  validate_obs_tab(obs_tab)

  observations <- obs_tab %>%
    filter(evid %in% c(0,2))

  cmt_in_obstab <- unique(observations$cmt)

  if(length(cmt_in_obstab) == 1){
    observations$name <- unique(predictions$name) #either DV, but PARENT alone also...
  } else {
    observations$name <- ""
    observations$name[observations$cmt == min(cmt_in_obstab)] <- "PAR"
    observations$name[observations$name != "PAR"] <- "MET"
    observations$name <- factor(observations$name, c("PAR", "MET"))
  }

  #MDV
  if(any(observations$mdv == 1)){
    gg <- gg +
      geom_point(
        data = observations %>%
          mutate(MDV = factor(.data$mdv, levels = c(0,1))),
        mapping = aes(
          y = .data$DV,
          shape = .data$MDV
        ),
        fill = "black", size = 3) +
      scale_shape_manual(values= c(`0` = 21, `1` = 1))
  } else {
    gg <- gg +
      geom_point(data = observations, aes(y = .data$DV), fill = "black", size = 3, pch = 21)
  }

  #Facetting

  one_cmt <- (length(unique(predictions$name)) == 1) & (length(unique(observations$name)) == 1)
  one_ID <- (length(unique(predictions$ID)) == 1) & (length(unique(observations$ID)) == 1)

  if(all(!one_cmt, !one_ID)) {
    gg <- gg+
      facet_grid(ID~name, scales = "free", labeller = label_both)
  }

  if(all(one_cmt, !one_ID)) {
    gg <- gg+
      facet_grid(ID~., scales = "free", labeller = label_both)
  }

  if(all(!one_cmt, one_ID)) {
    gg <- gg+
      facet_grid(.~name, scales = "free", labeller = label_both)
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
