#' Visual Predicted Checks
#'
#' @param x the model object
#' @param data NMTRAN-like data set
#' @param nrep a numeric, the number of replicates for stochastic simulations. Default is 500.
#' @param pcvpc a logical, if `TRUE` (the default) will output "prediction-corrected VPC" (see Details).
#' @param idv a character indicating the variable used as independant variable. Default is "time", alternatively use "tad" to automatically compute the time after last dose.
#' @param stratify_on a character (vector) indicating the variables of the data used to stratify the results. Variables must be numeric (as they are passed to [mrgsolve::carry_out()])
#' @param start,end,delta,... passed to [mrgsolve::mrgsim()]
#'
#' @details
#' * Prediction-corrected VPC
#'
#' By default, VPC are prediction corrected (Bergstrand et al (2011) <doi:10.1208/s12248-011-9255-z>.
#' This correction is advised if several levels of doses or covariates are in the dataset for instance.
#' Note that the implemented correction formula does not take into account the 'lower bound' term (*lbij*), nor the log-transformed variables.
#'
#' @return a ggplot2 object, results of the VPC. The median and the 50%, 80% and 90% prediction intervals of the simulated distributions are reported.
#' @export
#'
#' @examples
#' library(mrgsolve)
#' library(magrittr)
#' # Define a model. Adding variability to house model because default is 0.
#' mod <- house() %>%
#'   omat(dmat(rep(0.2,4)))
#'
#' # Creating dataset for the example
#' # Same concentration, but different dose (ID 2) and covariate (ID 3)
#' data <- adm_rows(ID = 1, amt = 1000, cmt = 1, addl = 6, ii = 12) %>%
#'   obs_rows(DV = 50, cmt = 2, time = 7 * 12) %>%
#'   adm_rows(ID = 2, time = 0, amt = 2000, cmt = 1, addl = 6, ii = 12) %>%
#'   obs_rows(DV = 50, cmt = 2, time = 7 * 12) %>%
#'   adm_rows(ID = 3, time = 0, amt = 1000, cmt = 1, addl = 6, ii = 12) %>%
#'   obs_rows(DV = 50, cmt = 2, time = 7 * 12) %>%
#'   add_covariates(SEX = c(0,0,0,0,1,1))
#'
#' mapbayr_vpc(mod, data) # prediction-corrected by default
#' mapbayr_vpc(mod, data, idv = "tad", start = 72)
#' mapbayr_vpc(mod, data, idv = "tad", start = 72, pcvpc = FALSE)
#' mapbayr_vpc(mod, data, idv = "tad", start = 72, stratify_on = "SEX")
mapbayr_vpc <- function(x,
                        data = NULL,
                        nrep = 500,
                        pcvpc = TRUE,
                        idv = "time",
                        stratify_on = NULL,
                        start = NULL,
                        end = NULL,
                        delta = 1,
                        ...){
  vpc_plot(
    vpc_sim(
      x = x,
      data = data,
      nrep = nrep,
      pcvpc = pcvpc,
      idv = idv,
      stratify_on = stratify_on,
      start = start,
      end = end,
      delta = delta,
      ... = ...
    )
  )
}

vpc_sim <- function(x,
                    data = NULL,
                    nrep = 500,
                    pcvpc = TRUE,
                    idv = "time",
                    stratify_on = NULL,
                    start = NULL,
                    end = NULL,
                    delta = 1,
                    ...){

  # Data
  if(is.null(data)){
    data <- x@args$data
  }
  x@args$data <- NULL #empty the data slot in model object

  # Arguments used in iteration
  sim_args <- prepare_vpc_sim(
    data = data,
    delta = delta,
    end = end,
    start = start
    )

  # Request
  if(any(c("PAR", "MET") %in% outvars(x)$capture)){
    Request <- c("PAR", "MET")
  } else {
    Request <- "DV"
  }

  # Check stratification
  if(!all(sapply(data[,stratify_on], is.numeric))){
    stop("Variables defined with `stratify_on` are not all numeric")
  }

  # Perform stochastic simulations
  stochastic_sim <- bind_rows(
    pmap(
      .l = sim_args,
      .f = do_mapbayr_sim,
      x = x,
      carry_out = c("a.u.g", stratify_on, idv),
      Request = Request,
      tad = idv == "tad",
      ... = ...,
      new_omega = NULL, #stochastic simulations
      nrep = nrep # with n replicates
    )
  ) %>%
    pivot_sims()

  # Prepare OBSERVED data
  OBSTAB <- data

  ## Define independant variable (tad, time...)

  keep_aug_0 <- stochastic_sim$a.u.g == 0
  keep_firstreplicate <- stochastic_sim$ID == 1
  keep_uniqueoutput <- stochastic_sim$name == stochastic_sim$name[1]

  OBSTAB$idv <- stochastic_sim[[idv]][keep_aug_0 & keep_firstreplicate & keep_uniqueoutput]

  ## Reframe (keep evid=0, name/value with PAR/MET)
  OBSTAB <- reframe_observations(OBSTAB)

  ## Define bins
  OBSTAB$bin <- define_bin(OBSTAB, stratify_on = stratify_on, delta = delta)


  # Prepare SIMULATED data

  SIMTAB <- stochastic_sim[stochastic_sim$a.u.g == 1, ]
  SIMTAB$idv <- SIMTAB[[idv]]
  SIMTAB$bin <- define_bin(SIMTAB, stratify_on = stratify_on, delta = delta)

  if(pcvpc){
    # Simulated typical profiles
    typical_sim <- bind_rows(
      pmap(
        .l = sim_args,
        .f = do_mapbayr_sim,
        x = x,
        carry_out = c("a.u.g", stratify_on, idv),
        Request = Request,
        tad = idv == "tad",
        ... = ...,
        new_omega = "zero_re", # typical simulations
        nrep = NULL # no replicates
      )
    ) %>%
      pivot_sims()

    # Define bins

    typical_sim$idv <- typical_sim[[idv]]
    typical_sim[["bin"]][typical_sim$a.u.g==0] <- define_bin(typical_sim[typical_sim$a.u.g==0,], stratify_on = stratify_on, delta = delta)
    typical_sim[["bin"]][typical_sim$a.u.g==1] <- define_bin(typical_sim[typical_sim$a.u.g==1,], stratify_on = stratify_on, delta = delta)

    # Median PRED tab --> PREDbin variable

    medpredtab <- typical_sim %>%
      filter(.data$a.u.g == 1) %>%
      group_by(.data$bin) %>%
      summarise(PREDbin = stats::median(.data$value))

    # Apply correction, on SIMTAB

    SIMTAB <- SIMTAB %>%
      left_join(medpredtab, by = "bin") %>%
      arrange(.data$ID) %>%
      mutate(value = predcorr(
        Yij = .data$value,
        PREDbin = .data$PREDbin,
        PREDij = rep(typical_sim$value[typical_sim$a.u.g==1], nrep)
      )
      )
    # Apply correction, on OBSTAB

    OBSTAB <- OBSTAB %>%
      left_join(medpredtab, by = "bin") %>%
      mutate(value = predcorr(
        Yij = .data$value,
        PREDbin = .data$PREDbin,
        PREDij = filter(
          typical_sim,
          .data$a.u.g == 0,
          .data$name == unique(.data$name)[1]
        )$value[data$evid %in% c(0, 2)]
      )
      )
  }

  list(
    SIMTAB = SIMTAB,
    OBSTAB = OBSTAB,
    stratify_on = stratify_on,
    idv = idv
  )

}

vpc_plot <- function(vpc_sim, stratify_on = vpc_sim$stratify_on, idv = vpc_sim$idv){
  dataplot <- vpc_sim$SIMTAB %>%
    group_by(across(all_of(c("bin", "idv", "name", vpc_sim$stratify_on)))) %>%
    summarise(
      Q05 = stats::quantile(.data$value, 0.05),
      Q10 = stats::quantile(.data$value, 0.10),
      Q25 = stats::quantile(.data$value, 0.25),
      Q50 = stats::quantile(.data$value, 0.50),
      Q75 = stats::quantile(.data$value, 0.75),
      Q90 = stats::quantile(.data$value, 0.90),
      Q95 = stats::quantile(.data$value, 0.95)
    )

  p <- dataplot %>%
    ggplot(aes(.data$idv)) +
    geom_line(aes(y = .data$Q50)) +
    geom_ribbon(aes(ymin = .data$Q05, ymax = .data$Q95), alpha = .2, fill = "blue") +
    geom_ribbon(aes(ymin = .data$Q10, ymax = .data$Q90), alpha = .2, fill = "blue") +
    geom_ribbon(aes(ymin = .data$Q25, ymax = .data$Q75), alpha = .2, fill = "blue") +
    geom_point(aes(y = .data$value), data = vpc_sim$OBSTAB) +
    labs(x = idv, y = "value")

  strats <- character(0)

  if(length(unique(vpc_sim$SIMTAB$name)) != 1){
    strats <- c(strats, "name")
  }

  if(!is.null(stratify_on)){
    strats <- c(strats, stratify_on)
  }

  if(length(strats) >= 1){
    p <- p +
      facet_wrap(strats, labeller = label_both)
  }

  p
}

predcorr <- function(Yij, PREDbin, PREDij){

  # Yij = (simulated or observed) dependant variable
  # PREDij = typical predicted depenant variable
  # PREDbin = median of typical predictions in the given bin

  # As described by Bergstrand et al, AAPS J 2011
  # https://link.springer.com/article/10.1208/s12248-011-9255-z

  pcYij <- Yij * PREDbin / PREDij
  pcYij[Yij==0 & PREDij==0] <- 0
  pcYij
}

define_bin <- function(x, stratify_on, delta = delta){
  # x = a dataset
  # stratify_on = character vector, variables to stratify on
  # delta = delta used in simulations, useful here for the binning
  x$roundidv <- floor(x$idv / delta) * delta
  ans <- do.call(interaction, list(x[, c("roundidv", "name" , stratify_on)], sep = "_"))
  return(as.character(ans))
}

prepare_vpc_sim <- function(data, start = NULL, end = NULL, delta = 1){
  # Prepare the arguments that will be passed to pmap(.f = do_mapbayr_sim)

  # data = full dataset
  # start, end, delta = passed to mrgsim

  args <- tibble::tibble(data = split_mapbayr_data(data))
  args <- mutate(args, ORIGID = names(data), .before = 1)
  args$tgrid <- sapply(args$data, infer_tgrid, delta = delta, end = end, start = start)
  args$obsaug <- TRUE
  args$new_sigma <- "zero_re"

  args
}
