#' Make mapbayr plot
#'
#' @param aug_tab a table of predictions, generated by `augment(x)` and available at `x$aug_tab`
#' @param obs_tab a table of observations
#' @param PREDICTION plot either "IPRED", "PRED" or both.
#' @param MODEL_color a vector of strings interpretable as colors, with names that correspond to a value in `aug_tab$MODEL`
#'
#' @return a `ggplot` object
#' @export
#'
#' @examples
#' aug <- data.frame(
#'   ID = 1, name = factor("DV"), cmt = 2, time = rep(c(0,8,16,24), each = 2),
#'   type = rep(c("PRED", "IPRED"), 4), value = c(0,0, 1, 2, 4, 8, 2, 4)
#'   )
#'
#' obs <- data.frame(
#'   ID = 1, time = c(6, 20), evid = 0,
#'   mdv = c(0,1), DV = c(0.5, 5), cmt = 2
#'   )
#'
#' mapbayr_plot(aug, obs)
#' mapbayr_plot(aug, obs, PREDICTION = "IPRED")
#'
#' aug2 <- dplyr::bind_rows(
#'   FOO = aug,
#'   BAZ = dplyr::mutate(aug, value = value * 2),
#'   BAR = dplyr::mutate(aug, value = value * 3),
#'   .id = "MODEL"
#'   )
#'
#' mapbayr_plot(aug2, obs)
#' mapbayr_plot(aug2, obs, PREDICTION = "IPRED")
#' mapbayr_plot(aug2, obs, PREDICTION = "IPRED", MODEL_color = c(FOO = "black"))
#'
#'
mapbayr_plot <- function(aug_tab, obs_tab = NULL, PREDICTION = c("IPRED", "PRED"), MODEL_color = NULL){

  # Predictions table

  validate_aug_tab(aug_tab)

  predictions <- aug_tab %>%
    filter(.data$type %in% PREDICTION) %>%
    mutate(PREDICTION = .data$type)

  predictions_names <- sort(unique(predictions$name)) # either DV, PAR, MET, or PAR+MET

  # Observations table

  if(!is.null(obs_tab)){
    observations <- reframe_observations(
      obs_tab = obs_tab,
      predictions_names = predictions_names
    )
  } else {
    observations <- NULL
  }

  # ggplot object

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

    coloration_values <- model_coloration(
      model_names = unique(predictions$MODEL),
      forced_colorations = MODEL_color
    )
  }

  gg <- predictions %>%
    ggplot(aes(.data$time, .data$value)) +
    geom_line(aes_lines) +
    theme_bw() +
    theme(
      legend.position = "bottom",
      strip.background = element_rect(fill = "white")
    ) +
    scale_color_manual(values = coloration_values) +
    scale_linetype_manual(values = c(IPRED = 1, PRED = 2))

  if(!is.null(predictions[["value_low"]]) & !is.null(predictions[["value_up"]])){
    data_ribbon <- predictions %>%
      filter(!(is.na(.data$value_low) & is.na(.data$value_up)))

    gg <- gg +
      geom_ribbon(aes_ribbon, data = data_ribbon, alpha = 0.3) +
      scale_fill_manual(values = coloration_values)
  }

  #MDV
  if(!is.null(observations)){
    if(any(observations$mdv == 1)){
      gg <- gg +
        geom_point(
          mapping = aes(shape = .data$MDV),
          data = observations,
          fill = "black",
          size = 3
        ) +
        scale_shape_manual(values= c(`0` = 21, `1` = 1))
    } else {
      gg <- gg +
        geom_point(data = observations, fill = "black", size = 3, pch = 21)
    }
  }

  #Facetting

  one_cmt <- (length(unique(predictions$name)) == 1) & (length(unique(observations$name)) <= 1)
  one_ID <- (length(unique(predictions$ID)) == 1) & (length(unique(observations$ID)) <= 1)

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
    all(c("type", "ID", "time", "name", "value") %in% names(x)),
    is.factor(x$name),
    all(levels(x$name) %in% c("DV", "PAR", "MET"))
  )

}

validate_obs_tab <- function(x){
  stopifnot(
    all(c("ID", "time", "evid", "DV", "cmt", "mdv") %in% names(x))
  )
}

model_coloration <- function(model_names, forced_colorations = NULL){
  if(!requireNamespace("scales", quietly = TRUE)) {
    stop(
      "Package \"scales\" must be installed to use the `MODEL_color` argument.",
      call. = FALSE
    )
  }

  model_names <- sort(model_names)
  cols <- scales::hue_pal()(length(model_names))
  names(cols) <- model_names

  if(!is.null(forced_colorations)){
    forced_colorations <- forced_colorations[sort(names(forced_colorations))]
    common <- intersect(names(cols), names(forced_colorations))
    cols[names(cols)%in%common] <- forced_colorations[names(forced_colorations) %in% common]
  }

  cols
}

reframe_observations <- function(obs_tab, predictions_names = NULL){

  validate_obs_tab(obs_tab)

  observations <- obs_tab

  observations <- observations[observations$evid %in% c(0,2), ]
  observations$MDV <- factor(observations$mdv, levels = c(0,1))

  cmt_in_obstab <- unique(observations$cmt)

  if(length(cmt_in_obstab) == 1){
    if(is.null(predictions_names)){
      observations[["name"]] <- "DV"
    } else {
      observations[["name"]] <- as.character(predictions_names[1])
    }
  } else {
    if(is.null(predictions_names)){
      predictions_names <- c("PAR", "MET")
    }
    observations[["name"]][observations$cmt == cmt_in_obstab[1]] <- as.character(predictions_names[1])
    observations[["name"]][observations$cmt == cmt_in_obstab[2]] <- as.character(predictions_names[2])
  }

  observations[["name"]] <- factor(observations$name, levels = c("DV", "PAR", "MET"))
  names(observations)[names(observations) == "DV"] <- "value"
  observations <- observations[!is.na(observations[["value"]]),]

  observations
}
