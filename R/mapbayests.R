#S3 Methods for `mapbayests` objects.

#' Print a mapbayests object
#'
#' @param x A \code{mapbayests} object.
#' @param ... additional arguments
#' @return print the results of the estimation to the console, and returns it invisibly.
#' @method print mapbayests
#' @export
print.mapbayests <- function(x, ...){
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
#' @param x A \code{mapbayests} object.
#' @param row.names,optional,... passed to as.data.frame
#'
#' @method as.data.frame mapbayests
#' @return a data.frame (the mapbay_tab from estimation)
#' @export
as.data.frame.mapbayests <- function(x, row.names = NULL, optional = FALSE, ...){
  as.data.frame(x$mapbay_tab, ...)
}


#' Plot predictions from mapbayests object
#'
#' @param x A \code{mapbayests} object.
#' @param ... additional arguments (not used)
#' @return a `ggplot` object. Observed and predicted concentration vs time profile for every patients.
#'
#' @method plot mapbayests
#' @export
plot.mapbayests <- function(x, ...){
  #  if(!inherits(x, "mapbayests")) stop("Provided object is not a mapbayests class object")

  if(is.null(x$aug_tab)){
    #  message("$aug_tab automatically provided. Consider executing augment() manually to save computational time or access options.")
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
    scale_color_manual(values= c(IPRED = "black", PRED = "deepskyblue1")) +
    scale_linetype_manual(values= c(IPRED = 1, PRED = 2))

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
#' @param x A \code{mapbayests} object.
#' @param ... additional arguments (not used)
#' @return a `ggplot` object, representing prior parameter density distribution, and a histogram of patients estimates.
#' @method hist mapbayests
#' @export
hist.mapbayests <- function(x, ...){

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
#' @return an augmented object (depending on the object passed)
augment <- function (x, ...)UseMethod("augment")

#' Compute full PK profile prediction from mapbayr estimates.
#'
#' @param x A \code{mapbayests} object.
#' @param data dataset to pass to mrgsolve for simulation (default is dataset used for estimation)
#' @param end end of simulation time (passed to mrgsim)
#' @param delta delta of simulation time (passed to mrgsim)
#' @param ... additional argument to pass to mrgsim
#'
#' @method augment mapbayests
#' @return a `mapbayests` object, augmented of an `aug_tab`
#' @export
augment.mapbayests <- function(x, data = NULL, end = NULL, delta = NULL,...){
  if(is.null(data)){
    data <- x$data
  }
  if(is.null(end)){
    end <- data %>%
      group_by(.data$ID) %>%
      slice_max(.data$time, with_ties = F) %>%
      pull(.data$time)
    #for each ID, a different end time. By default, +20% of last obs or dosing.
    end <- end * 1.2
    #end is then a vector of numeric
  }
  if(is.null(delta)){
    .min <- min(data$time)
    .max <- max(end)
    .delta <- (.max - .min)/200 #approximately 200 points per graph
    delta <- 10^(round(log10(abs(.delta)))) #rounded to the closer 10 (0.1, 1, 10 etc...)
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
        mrgsim_df(carry_out = carry, end = end, delta = delta, ...) %>%
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
        mrgsim_df(carry_out = carry, end = end, delta = delta, ...) %>%
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
  class(x) <- "mapbayests"
  return(x)
}

#' Use posterior param and covariates
#'
#' @param x A \code{mapbayests} object.
#' @param .zero_re Default is "both", meaning all matrices are zeroed. Pass "omega" to zero between-subject variability, and keep simulating residual error.
#'
#' @details Updates the param values of the model object with the estimated etas, and the covariates of the individual. Returns an updated mrgmod, so that the user can derive simulations from it. Works only with one individual. Does not handle time-varying covariates.
#' @return a mrgmod
#' @export
use_posterior <- function(x, .zero_re = c("both", "omega", "sigma")){
  mod <- x$model

  if(length(x$arg.ofv.id) > 1) stop("use_posterior() can be used with one only ID", call. = FALSE)

  mod <- switch (.zero_re[1],
                 "both" = zero_re(mod),
                 "omega" = zero_re(mod, "omega"),
                 "sigma" = zero_re(mod, "sigma")
  )

  covs_name <- mbr_cov_names(mod)
  covs_name <- covs_name[!covs_name%in%c("AOLA", "TOLA")]

  etas <- x$final_eta[[1]]
  is_tv <- (map_dbl(covs_name, ~length(unique(x$mapbay_tab[[.x]]))) != 1)

  if(any(is_tv)) warning("Time-varying covariates found. First value used for: ",  paste(covs_name[is_tv], collapse = ", "), ".")

  covs <- x$mapbay_tab[1,covs_name, drop = FALSE]

  mod %>%
    param(as.list(c(etas, covs)))
}
