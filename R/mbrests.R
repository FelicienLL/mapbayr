#S3 Methods for `mbrests` objects.

#' Print a mbrests object
#'
#' @param x A \code{mbrests} object.
#' @param ... additional arguments
#'
#' @method print mbrests
#' @export
print.mbrests <- function(x, ...){
  NAME <- x$model@model
  nID <- length(x$arg.ofv.id)
  nOBS <- x$arg.ofv.id %>% map("DVobs") %>% unname() %>% simplify() %>% length()
  nETA <- n_eta(x$model)
  ETA <- x$final_eta %>%
    bind_rows(.id = "ID") %>%
    as.data.frame() %>%
    utils::head()
  TAB <- utils::head(as.data.frame(x$mapbay_tab))

  cat("Model: ", NAME, "\n")
  cat("ID :", nID, " individual(s).\n")
  cat("OBS:", nOBS, " observation(s).\n")
  cat("ETA:", nETA, " parameter(s) to estimate.\n\n")
  cat("Estimates: \n")
  print(ETA)
  cat("\nOutput (", nrow(x$mapbay_tab) , " lines): \n", sep = "")
  print(TAB)
}


#' Return the mapbay_tab as a data.frame
#'
#' @param x A \code{mbrests} object.
#' @param row.names,optional,... passed to as.data.frame
#'
#' @method as.data.frame mbrests
#' @export
as.data.frame.mbrests <- function(x, row.names = NULL, optional = FALSE, ...){
  as.data.frame(x$mapbay_tab, ...)
}


#' Plot predictions from mbrests object
#'
#' @param x A \code{mbrests} object.
#' @param ... additional arguments (not used)
#'
#' @method plot mbrests
#' @export
plot.mbrests <- function(x, ...){
  #  if(!inherits(x, "mbrests")) stop("Provided object is not a mbrests class object")

  if(is.null(x$aug_tab)){
    message("$aug_tab automatically provided. Consider executing augment() manually to save computational time or access options.")
    x <- augment(x)
  }

  theme_custom <- function(...) {
    theme_bw(...) %+replace%
      theme(legend.position = "bottom",
            strip.background = element_rect(fill="white")
      )
  }

  predictions <- x$aug_tab %>%
    mutate(PREDICTION  = .data$type)

  gg <- predictions %>%
    ggplot(aes(.data$time, .data$value)) +
    geom_line(aes(col = .data$PREDICTION, linetype = .data$PREDICTION)) +
    theme_custom()+
    scale_color_manual(values= c(PRED = "deepskyblue1", IPRED = "black"))

  observations <- x$mapbay_tab %>%
    filter(.data$evid==0) %>%
    mutate(MDV = as.factor(.data$mdv))

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

  one_cmt <- length(obs_cmt(x$model)) == 1
  one_ID <- length(x$arg.ofv.id) == 1

  if(all(!one_cmt, !one_ID)) {
    gg <- gg+
      facet_grid(rows = vars(.data$ID), cols = vars(.data$cmt), scales = "free", labeller = label_both)
  }

  if(all(one_cmt, !one_ID)) {
    gg <- gg+
      facet_grid(rows = vars(.data$ID), scales = "free", labeller = label_both)
  }

  if(all(!one_cmt, one_ID)) {
    gg <- gg+
      facet_grid(cols = vars(.data$cmt), scales = "free", labeller = label_both)
  }

  return(gg)

}

#' Plot posterior distribution of bayesian estimates
#'
#' @param x A \code{mbrests} object.
#' @param ... additional arguments (not used)
#'
#' @method hist mbrests
#' @export
hist.mbrests <- function(x, ...){

  # --- Eta tab
  eta_tab <- x$final_eta %>%
    bind_rows() %>%
    pivot_longer(everything())

  # --- Arguments tab
  arg_tab <- data.frame(
    om = odiag(x$model),
    name = eta_names(x$model),
    descr = eta_descr(x$model),
    lower = x$arg.optim$lower,
    upper = x$arg.optim$upper
  )

  # --- Density tab
  minlow <- min(arg_tab$lower)
  maxup <- max(arg_tab$upper)
  xvalues <- seq(minlow - 0.01, maxup + 0.01, 0.01)

  density_tab <- arg_tab %>%
    select(.name = .data$name,.om = .data$om) %>%
    pmap_dfr(function(.name, .om){
      data.frame(name = .name,
                 x = xvalues,
                 value = dnorm(xvalues, mean = 0, sd = sqrt(.om)))
    })

  # --- Labels
  eta_labs <- paste0(arg_tab$descr,
                     "\nIIV = ", my_percent(sqrt(arg_tab$om)))
  # --- one ID
  if(length(x$final_eta) == 1){
    percentile <- map2_dbl(x$final_eta[[1]], sqrt(arg_tab$om), pnorm, mean = 0)
    eta_labs <- paste0(eta_labs,
                       "\nID percentile = ", my_percent(percentile))
  }

  names(eta_labs) <- arg_tab$name

  ggplot() +
    facet_wrap("name", labeller = labeller(name = eta_labs)) +
    geom_area(aes(x = .data$x, y = .data$value), data = density_tab, fill = "skyblue", alpha = .3) +
    geom_line(aes(x = .data$x, y = .data$value), data = density_tab) +
    geom_segment(aes(x = .data$lower, xend = .data$lower), y = -0.03, yend = .1, data = arg_tab, linetype = 1, size = 1) +
    geom_segment(aes(x = .data$upper, xend = .data$upper), y = -0.03, yend = .1, data = arg_tab, linetype = 1, size = 1) +
    theme_bw() +
    theme(strip.background = element_rect(fill = "white"))+
    scale_y_continuous(name = NULL, breaks = NULL, labels = NULL)+
    scale_x_continuous(name = NULL, n.breaks = 10)+
    coord_cartesian(ylim = c(NA, max(density_tab$value)))+
    geom_rug(aes(x = .data$value), data = eta_tab)+
    geom_histogram(aes(x = .data$value, y = .data$..density..), data = eta_tab, alpha = .8, col = 'black', bins = 50)
}


#' Compute full PK profile prediction from mapbayr estimates.
#' @param x object to augment
#' @param ... additional arguments
#' @export
augment <- function (x, ...)UseMethod("augment")

#' Compute full PK profile prediction from mapbayr estimates.
#'
#' @param x A \code{mbrests} object.
#' @param data dataset to pass to mrgsolve for simulation (default is dataset used for estimation)
#' @param end end of infusion time (passed to mrgsim)
#' @param ... additional argument to pass to mrgsim
#'
#' @method augment mbrests
#' @return a `mbrests` object, augmented of an `aug_tab`
#' @export
augment.mbrests <- function(x, data = NULL, end = NULL, ...){
  if(is.null(data)){
    data <- x$data
  }
  if(is.null(end)){
    end <- data %>%
      group_by(.data$ID) %>%
      slice_max(.data$time, with_ties = F) %>%
      pull(.data$time)
    end <- end+24
  }

  carry <- data %>%
    select(-any_of(c("ID", "time", "cmt","DV"))) %>%
    names()

  idata <- data %>%
    check_mapbayr_data() %>%
    split_mapbayr_data()

  ipred <- list(data = idata,
                end = end,
                eta = x$final_eta) %>%
    pmap_dfr(function(data, end, eta, ...){
      x$model %>%
        param(eta) %>%
        zero_re() %>%
        data_set(data) %>%
        obsaug() %>%
        mrgsim_df(carry_out = carry, end = end, ...) %>%
        as_tibble() %>%
        filter(.data$evid %in% c(0,2)) %>%
        select(-any_of(x$model@cmtL)) %>%
        mutate(type = "IPRED")
    }, ... = ...)

  pred <- list(data = idata,
               end = end) %>%
    pmap_dfr(function(data, end, eta, ...){
      x$model %>%
        zero_re() %>%
        data_set(data) %>%
        obsaug() %>%
        mrgsim_df(carry_out = carry, end = end, ...) %>%
        as_tibble() %>%
        filter(.data$evid %in% c(0,2)) %>%
        select(-any_of(x$model@cmtL)) %>%
        mutate(type = "PRED")
    }, ... = ...)

  aug_tab <- bind_rows(ipred, pred)

  fitcmt <- fit_cmt(x$model, idata[[1]])

  if(length(fitcmt)>1){
    aug_tab <- select(aug_tab, -any_of(c("DV")))
  } else{
    aug_tab <- select(aug_tab, -any_of(c("PAR", "MET")))
  }

  aug_tab <- aug_tab %>%
    pivot_longer(any_of(c("DV", "PAR", "MET"))) %>%
    mutate(cmt = ifelse(.data$name %in% c("DV", "PAR"), fitcmt[1], fitcmt[2]))%>%
    arrange(.data$ID, .data$time, .data$cmt, .data$type)

  x <- c(x, aug_tab = list(aug_tab))
  class(x) <- "mbrests"
  return(x)
}
