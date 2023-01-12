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
  nOBS <- x$arg.ofv.id %>% map("idDV") %>% unname() %>% simplify() %>% length()
  nETA <- eta_length(x$model)
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
#' @param ... additional arguments (passed to \code{\link{augment.mapbayests}})
#' @param PREDICTION plot either "IPRED", "PRED" or both.
#'
#' @return a `ggplot` object.
#'
#' @details
#' Use this function to plot the results of the estimations, in the form of concentration vs time profiles for every patient of the data set.
#' For additional modifications, you can:
#'  - see \code{\link{augment.mapbayests}} to modify the simulation output.
#'  - add extra `+function(...)` in order to modify the plot as a regular `ggplot2` object.
#'
#' @examples
#' est <- mapbayest(exmodel(ID = 1))
#' plot(est, end = 48) +
#'   ggplot2::labs(title = "Awesome prediction")
#'
#'
#' @method plot mapbayests
#' @export
plot.mapbayests <- function(x, ..., PREDICTION = c("IPRED", "PRED")){
  #  if(!inherits(x, "mapbayests")) stop("Provided object is not a mapbayests class object")

  if(is.null(x$aug_tab)){
    #  message("$aug_tab automatically provided. Consider executing augment() manually to save computational time or access options.")
    x <- augment(x, ...)
  }

  theme_custom <- function(...) {
    theme_bw(...) %+replace%
      theme(legend.position = "bottom",
            strip.background = element_rect(fill="white")
      )
  }

  predictions <- x$aug_tab %>%
    filter(.data$type %in% PREDICTION) %>%
    mutate(PREDICTION  = .data$type)

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

  observations <- x$mapbay_tab %>%
    filter(.data$evid==0, !(.data$mdv==1 & is.na(.data$DV))) %>%
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

#' Plot posterior distribution of bayesian estimates
#'
#' @param x A \code{mapbayests} object.
#' @param ... additional arguments (not used)
#' @return a `ggplot` object.
#'
#' @details
#' Use this function to plot the results of the estimations, in the form of histograms with the *a priori* distribution in the background. For every parameter, the inter-individual variability is displayed, as well as the percentile of the patient in the corresponding distribution (if n = 1 patient).
#' For additional modifications, you can add extra `+function(...)` in order to modify the plot as a regular `ggplot2` object.
#'
#' @examples
#' est <- mapbayest(exmodel(ID = 1))
#' hist(est) +
#'   ggplot2::labs(title = "Awesome estimations")
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
    descr = eta_descr(x$model)
    )

  if(is.null(x$arg.optim$select_eta)){ # backward compatibility
    x$arg.optim$select_eta <- seq_len(nrow(arg_tab))
  }
  arg_tab$lower <- rep(NA_real_, nrow(arg_tab))
  arg_tab$lower[x$arg.optim$select_eta] <- x$arg.optim$lower

  arg_tab$upper <- rep(NA_real_, nrow(arg_tab))
  arg_tab$upper[x$arg.optim$select_eta] <- x$arg.optim$upper

  # --- Density tab
  minlow <- min(arg_tab$lower, na.rm = TRUE)
  maxup <- max(arg_tab$upper, na.rm = TRUE)
  xvalues <- seq(minlow - 0.01, maxup + 0.01, 0.01)

  density_tab <- arg_tab %>%
    select(.name = .data$name,.om = .data$om) %>%
    pmap_dfr(function(.name, .om){
      data.frame(name = .name,
                 x = xvalues,
                 value = stats::dnorm(xvalues, mean = 0, sd = sqrt(.om)))
    })

  # --- Labels
  eta_labs <- paste0(arg_tab$descr,
                     "\nIIV = ", my_percent(sqrt(arg_tab$om)))
  # --- one ID
  if(length(x$final_eta) == 1){
    percentile <- map2_dbl(x$final_eta[[1]], sqrt(arg_tab$om), stats::pnorm, mean = 0)
    eta_labs <- paste0(eta_labs,
                       "\nID percentile = ", my_percent(percentile))
  }

  names(eta_labs) <- arg_tab$name

  ggplot() +
    facet_wrap("name", labeller = labeller(name = eta_labs)) +
    geom_area(aes(x = .data$x, y = .data$value), data = density_tab, fill = "skyblue", alpha = .3) +
    geom_line(aes(x = .data$x, y = .data$value), data = density_tab) +
    geom_segment(aes(x = .data$lower, xend = .data$lower), y = -0.03, yend = .1, data = arg_tab, linetype = 1, size = 1, na.rm = TRUE) +
    geom_segment(aes(x = .data$upper, xend = .data$upper), y = -0.03, yend = .1, data = arg_tab, linetype = 1, size = 1, na.rm = TRUE) +
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
#' @return an augmented object (depending on the object passed).
augment <- function (x, ...) UseMethod("augment")

#' Compute full PK profile prediction from mapbayr estimates.
#'
#' @param x A \code{mapbayests} object.
#' @param data dataset to pass to mrgsolve for simulation (default is dataset used for estimation)
#' @param start,end,delta start, end and delta of simulation time passed to `mrgsim()` (see details)
#' @param ci a logical. If TRUE, compute a confidence interval around the prediction (default is FALSE)
#' @param ci_width a number between 0 and 100, width of the confidence interval (default is "90" for a 90%CI)
#' @param ci_method method to compute the confidence interval. Can be "delta" (the default) to use the Delta approximation. Alternatively "simulations" for a more accurate approach, but also more time-consuming.
#' @param ci_sims number of replicates to simulate in order to derive the confidence interval (default is 500)
#' @param ... additional arguments passed to `mrgsim()`
#'
#' @method augment mapbayests
#' @return a `mapbayests` object, augmented of an `aug_tab` data.frame.
#' @details
#' This function is called in the background by `plot()` in order to simulate the full PK profile, and return a `mapbayests` object with an additional `aug_tab` data.frame inside. The latter is used with by the plot method.
#' The time grid, for each PK profile (i.e. patient) is defaulted with the minimum time in the dataset for `start` and the maximum time in the dataset +20% for `end`. `delta` is a power of 10 (e.g. 0.1, 1, 10 etc...), automatically chosen to render visually appealing graphs with a reasonable computing time (about 200 time points).
#' Additional arguments can be passed to `mrgsim()` through `...`. Note that `recsort` is set to 3 (see mrgsolve documentation for more details).
#'
#' @examples
#' #x is the result of `mapbayest()`.
#' #Default plot is returned by:
#' # plot(x)
#' #Argument passed to `plot()` are passed to `augment()` in the background:
#' # plot(x, end = 240, ci = TRUE)
#' #Save the augmented object if simulation time is long
#' # x2 <- augment(x, ci = TRUE, ci_method = "simulations", ci_sims = 10000) %>%
#' # plot(x2)
#'
#' @export
augment.mapbayests <- function(x, data = NULL, start = NULL, end = NULL, delta = NULL, ci = FALSE, ci_width = 90, ci_method = "delta", ci_sims = 500, ...){
  if(is.null(data)){
    idata <- get_data.mapbayests(x, output = "list")
  } else {
    idata <- data %>%
      check_mapbayr_data() %>%
      split_mapbayr_data()
  }

  if(is.null(start)){
    start <- unname(sapply(idata, function(x) min(x$time)))
    #A vector. For each ID, possibly a different start time.
  }

  if(is.null(end)){
    end <- unname(sapply(idata, function(x) max(x$time)))
    #A vector. For each ID, possibly a different end time.
    end <- end * 1.2
    #By default, +20% of last obs or dosing.
  }

  if(is.null(delta)){
    .delta <- (end - start)/200 #approximately 200 points per graph
    delta <- 10^(round(log10(abs(.delta)))) #rounded to the closer 10 (0.1, 1, 10 etc...)
    #A vector. For each ID, possibly a different delta.
  }

  fitcmt <- fit_cmt(x$model, get_data(x))

  if(length(fitcmt) > 1){
    toreq <- "PAR,MET"
  } else {
    toreq <- "DV"
  }

  tocarry <- c("evid")

  do_sims <- function(mods, data = idata, ...){
    sims <- list(
      x = mods,
      data = data, start = start, end = end, delta = delta
    ) %>%
      pmap(mrgsim_df, carry_out = tocarry, Request = toreq, recsort = 3, obsaug = TRUE, ... = ...)

    map(sims, ~ .x %>%
      filter(.data$evid %in% c(0,2)) %>%
      select(-any_of("cmt")) %>% #should not be carried normally but who knows...
      pivot_longer(any_of(c("DV", "PAR", "MET"))) %>%
      mutate(cmt = ifelse(.data$name %in% c("DV", "PAR"), fitcmt[1], fitcmt[2])) %>%
      arrange(.data$ID, .data$time, .data$cmt)
      )
  }

  do_augment <- function(x, type, ...){
    stopifnot(type %in% c("ipred", "pred"))

    mods <- switch(type,
                   "ipred" = use_posterior(x, update_eta = TRUE, update_omega = TRUE, update_cov = FALSE, simplify = FALSE),
                   "pred" = use_posterior(x, update_eta = FALSE, update_omega = FALSE, update_cov = FALSE, simplify = FALSE, .zero_re = "sigma"))

    initpreds <- do_sims(map(mods, zero_re), ... = ...)

    if(ci){
      if(ci_method == "delta"){
        etanames <- eta_names(mods[[1]])
        init_etas <- map(mods, ~unlist(param(.x)[etanames]))
        dsteps <- log(1+1e-8) #directly add 1e-8 because working on eta so will return into a multiplicative effect on parameter scale
        new_etas <- map2(init_etas, dsteps, ~.x+.y)
        new_models_etas <- map2(mods, new_etas, function(M,E){
          map(seq_along(E), ~zero_re(param(M, E[.x]))) %>% set_names(etanames)
        }) %>% transpose()
        new_preds <- map(new_models_etas, do_sims,  ... = ...) %>% transpose() #1 item = 1 indiv
        jacobians <- list(new_preds, dsteps, initpreds) %>% #1 item = 1 indiv
          pmap(function(new, step, ini){
            map2_dfc(new, step, ~(.x[["value"]]-ini[["value"]])/.y) %>% as.matrix()
          })
        varcovs <- map(mods, omat, make = TRUE) #IIV or uncertainty, depending on the update
        errors <- map2(jacobians, varcovs, ~ znorm(ci_width) * sqrt(diag(.x %*% .y %*% t(.x))))
        initpreds <- map2(initpreds, errors, ~mutate(.x,
                                                     value_low = .data[["value"]] - .y,
                                                     value_up = .data[["value"]] + .y))
      }

      if(ci_method == "simulations"){
        new_idatas <- map(idata, data_nid, n = ci_sims)
        new_sims <- do_sims(mods, data = new_idatas, ... = ...)
        LOW <- map(new_sims, ~ .x %>% prepare_summarise() %>% summarise(v = quantile(.data$value, ci2q(ci_width))) %>% pull("v"))
        UP <- map(new_sims, ~ .x %>% prepare_summarise() %>% summarise(v = quantile(.data$value, 1-ci2q(ci_width))) %>% pull("v"))
        initpreds <- pmap(list(initpreds, LOW, UP), function(ini, low, up){
          mutate(ini, value_low = low, value_up = up)
        })
      }
    }
    initpreds
  }

  ipred <- do_augment(x, type = "ipred", ... = ...) %>% bind_rows()
  pred  <- do_augment(x, type = "pred", ... = ...) %>% bind_rows()

  x$aug_tab <- bind_rows(list(IPRED = ipred, PRED = pred), .id = "type") %>%
    as_tibble() %>%
    arrange(.data$ID, .data$time, .data$cmt, .data$type)

  class(x) <- "mapbayests"
  return(x)
}

data_nid <- function(data, n){
  bind_rows(lapply(seq_len(n), function(x) mutate(data, ID = x)))
}

prepare_summarise <- function(data){
  data %>%
    group_by(.data$ID) %>%
    mutate(rowID = rank(.data$ID, ties.method = "first")) %>%
    group_by(.data$rowID)
}
