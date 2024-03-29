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
  max_n_eta <- eta_length(x$model)
  nETA <- length(x$arg.optim$select_eta)

  ETA <- get_eta(x, x$arg.optim$select_eta, output = "df") %>%
    utils::head()
  TAB <- utils::head(x$mapbay_tab)

  cat("Model:", NAME, "\n")
  cat("ID :", nID, "individual(s).\n")
  cat("OBS:", nOBS, "observation(s).\n")
  cat("ETA:", nETA, "parameter(s) to estimate.\n")
  cat("\n")
  cat("Estimates: \n")
  print.data.frame(ETA)
  cat("\nOutput (", nrow(x$mapbay_tab) , " lines): \n", sep = "")
  print.data.frame(TAB, digits = 3)
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

  if(is.null(x$aug_tab)){
    x <- augment(x, ...)
  }

  mapbayr_plot(
    x$aug_tab,
    filter(x$mapbay_tab, .data$evid==0, !(.data$mdv==1 & is.na(.data$DV))),
    PREDICTION = PREDICTION
  )

}

#' Plot posterior distribution of bayesian estimates
#'
#' @param x A \code{mapbayests} object.
#' @param select_eta a vector of numeric values, the numbers of the ETAs to show (default are estimated ETAs).
#' @param shk method to compute the shrinkage if multiple subjects are analyzed. Possible values are "sd" (based on the ratio of standard deviation like in 'NONMEM'), "var" (based on the ratio of variances like 'Monolix'), or NA (do not show the shrinkage)
#' @param ... additional arguments (not used)
#' @return a `ggplot` object.
#'
#' @details
#' Use this function to plot the results of the estimations, in the form of histograms with the *a priori* distribution in the background. For every parameter, the inter-individual variability is displayed, as well as the percentile of the patient in the corresponding distribution (if n = 1 patient).
#' For additional modifications, you can add extra `+function(...)` in order to modify the plot as a regular `ggplot2` object.
#'
#' @examples
#' \donttest{
#' est <- mapbayest(exmodel(ID = 1))
#'
#' # Default Method
#' h <- hist(est)
#'
#' # Can be modified with `ggplot2`
#' h +
#'   ggplot2::labs(title = "Awesome estimations")
#'
#' # Select the ETAs
#' hist(est, select_eta = c(1,3))
#'}
#' @method hist mapbayests
#' @export
hist.mapbayests <- function(x,
                            select_eta = x$arg.optim$select_eta,
                            shk = c("sd", "var", NA),
                            ...){

  max_eta <- eta_length(x$model)
  select_eta_est <- x$arg.optim$select_eta

  # --- What ETA do we want?
  if(is.null(select_eta_est)){ # For backward compatibility (< 0.9)
    select_eta_est <- seq_len(max_eta)
  }

  select_eta_hist <- select_eta

  if(is.null(select_eta_hist)){
    select_eta_hist <- select_eta_est
  }

  if(any(select_eta_hist > max_eta)){
      stop("Cannot select ", paste(make_eta_names(select_eta_hist[select_eta_hist>max_eta]), collapse = " "),
           ": maximum ", max_eta, " ETAs defined in $PARAM.")
  }

  # --- Arguments tab
  # First on all ETAs for simplicity of indexation
  arg_tab <- data.frame(
    om = odiag(x$model),
    name = eta_names(x$model),
    descr = eta_descr(x$model)
    )

  # Default bounds to 0.1% - 99.9%
  bound <- get_quantile(x$model, .p = 0.001)
  arg_tab$lower <- bound
  arg_tab$upper <- -bound

  # Updates bound with those currently used in estimation if ever
  arg_tab$lower[select_eta_est] <- x$arg.optim$lower
  arg_tab$upper[select_eta_est] <- x$arg.optim$upper

  # Then filter the selected ETAs for the plot
  arg_tab <- arg_tab[select_eta_hist,]

  # --- Eta tab
  eta_tab <- get_eta(x, output = "df") %>%
    select(all_of(arg_tab$name)) %>%
    pivot_longer(everything())

  # --- Density tab
  minlow <- min(arg_tab$lower, na.rm = TRUE)
  maxup <- max(arg_tab$upper, na.rm = TRUE)
  xvalues <- seq(minlow - 0.01, maxup + 0.01, 0.01)

  density_tab <- data.frame(
    name = rep(arg_tab$name, each = length(xvalues)),
    x = rep(xvalues, length(arg_tab$name)),
    value = unlist(lapply(X = sqrt(arg_tab$om), FUN = stats::dnorm, x = xvalues, mean = 0))
  )

  # --- Labels
  eta_labs <- paste0(arg_tab$descr,
                     "\nIIV = ", my_percent(sqrt(arg_tab$om)))

  # --- one ID
  if(length(x$final_eta) == 1){
    percentile <- map2_dbl(get_eta.mapbayests(x, select_eta_hist, output = "num"),
                           sqrt(arg_tab$om),
                           stats::pnorm,
                           mean = 0)
    eta_labs <- paste0(eta_labs,
                       "\nID percentile = ", my_percent(percentile))
  } else {
    shk <- shk[1]
    if(!is.na(shk)){
      variances <- eta_tab %>%
        group_by(.data$name) %>%
        summarise(VAR = stats::var(.data$value)) %>%
        ungroup()
      shk_tab <- left_join(arg_tab, variances, by = "name")

      if(shk == "var"){
        shrinkage <- 1 - (shk_tab$VAR / shk_tab$om)
      }

      if(shk == "sd"){
        shrinkage <- 1 - (sqrt(shk_tab$VAR) / sqrt(shk_tab$om))
      }
      eta_labs <- paste0(eta_labs,
                         "\nSHK = ", my_percent(shrinkage))
    }
  }

  names(eta_labs) <- arg_tab$name
  density_tab$name <- factor(density_tab$name, arg_tab$name)
  eta_tab$name <- factor(eta_tab$name, arg_tab$name)
  arg_tab$name <- factor(arg_tab$name, arg_tab$name)

  ggplot() +
    facet_wrap("name", labeller = labeller(name = eta_labs)) +
    geom_area(aes(x = .data$x, y = .data$value), data = density_tab, fill = "skyblue", alpha = .3) +
    geom_line(aes(x = .data$x, y = .data$value), data = density_tab) +
    geom_segment(aes(x = .data$lower, xend = .data$lower), y = -0.03, yend = .1, data = arg_tab, linetype = 1, linewidth = 1, na.rm = TRUE) +
    geom_segment(aes(x = .data$upper, xend = .data$upper), y = -0.03, yend = .1, data = arg_tab, linetype = 1, linewidth = 1, na.rm = TRUE) +
    theme_bw() +
    theme(
      strip.background = element_rect(fill = "white")
      ) +
    scale_y_continuous(name = NULL, breaks = NULL, labels = NULL)+
    scale_x_continuous(name = NULL, n.breaks = 10)+
    coord_cartesian(ylim = c(NA, max(density_tab$value)))+
    geom_rug(aes(x = .data$value), data = eta_tab)+
    geom_histogram(aes(x = .data$value, y = after_stat(.data$density)), data = eta_tab, alpha = .8, col = 'black', bins = 50)
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
  # Data
  if(is.null(data)){
    data_list <- get_data.mapbayests(x, output = "list")
  } else {
    data_list <- data %>%
      check_mapbayr_data() %>%
      split_mapbayr_data()
  }

  # Confidence interval
  nrep <- NULL
  cov_list <- NULL
  ci_delta <- FALSE
  iiv_mat <- NULL
  if(ci == TRUE){
    cov_list <- get_cov(x, simplify = FALSE)
    if(grepl(pattern = "sim", x = ci_method)){
      nrep <- ci_sims
    }
    if(ci_method == "delta"){
      ci_delta <- TRUE
      iiv_mat <- omat(x$model, make = TRUE)
    }
  }

  # Arguments Table
  args_tab <- prepare_augment(
    data_list = data_list,
    eta_list = get_eta(x, output = "list"),
    cov_list = cov_list,
    ci_delta = ci_delta,
    start = start, end = end, delta = delta
  )

  # Request "PAR+MET" or "DV"?
  if(all(c("PAR", "MET") %in% outvars(x$model)$capture)){
    Request <- c("PAR", "MET")
  } else {
    Request <- "DV"
  }

  keys <- c("type", "ORIGID", "ci_delta")

  # Simulate
  sims <- args_tab %>%
    select(any_of(keys)) %>%
    mutate(sim = pmap( #mutate() in order to nest the results
      .l = select(args_tab, -any_of(keys)),
      .f = do_mapbayr_sim,
      x = x$model,
      recsort = 3,
      obsaug = TRUE,
      obsonly = TRUE,
      carry_out = "a.u.g",
      Request = Request,
      ... = ...,
      nrep = nrep,
      new_sigma = "zero_re"
    )
    )
  sims <- tidyr::unnest(data = sims, cols = "sim")

  # Post process
  ans <- reframe_augment(sims, cov_list = cov_list, iiv_mat = iiv_mat, ci_width = ci_width)

  x$aug_tab <- ans
  class(x) <- "mapbayests"
  return(x)
}
