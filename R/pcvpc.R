predcorr <- function(Yij, PREDbin, PREDij){
  pcYij <- Yij * PREDbin / PREDij
  pcYij[Yij==0 & PREDij==0] <- 0
  pcYij
}

define_bin <- function(x, stratify_on, delta = delta){
  x$roundidv <- floor(x$idv / delta) * delta
  ans <- do.call(interaction, list(x[, c("roundidv", "name" , stratify_on)], sep = "_"))
  return(as.character(ans))
}

prepare_vpc_sim <- function(data, delta = 1, end = NULL){
  args <- tibble::tibble(data = split_mapbayr_data(data))
  args <- mutate(args, ORIGID = names(data), .before = 1)
  args$tgrid <- sapply(args$data, infer_tgrid, delta = delta, end = end)
  args$obsaug <- TRUE
  args$new_sigma <- "zero_re"

  args
}

vpc_sim <- function(x,
                    data = NULL,
                    nrep = 20,
                    pcvpc = TRUE,
                    delta = 1,
                    idv = "tad",
                    end = NULL,
                    stratify_on = NULL,
                    ...){

  if(is.null(data)){
    data <- x@args$data
  }
  x@args$data <- NULL #empty the data slot in model object

  sim_args <- prepare_vpc_sim(data = data, delta = delta, end = end)

  if(any(c("PAR", "MET") %in% outvars(x)$capture)){
    Request <- c("PAR", "MET")
  } else {
    Request <- "DV"
  }

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

  OBSTAB <- data

  OBSTAB$idv <- filter(stochastic_sim,
    .data$a.u.g == 0,
    .data$ID == 1,
    .data$name == unique(.data$name)[1]
    )[[idv]]

  OBSTAB <- reframe_observations(OBSTAB)

  OBSTAB$bin <- define_bin(OBSTAB, stratify_on = stratify_on, delta = delta)

  stochastic_sim <- bind_rows(stochastic_sim) %>% as_tibble
  SIMTAB <- stochastic_sim[stochastic_sim$a.u.g == 1, ]
  SIMTAB$idv <- SIMTAB[[idv]]
  SIMTAB$bin <- define_bin(SIMTAB, stratify_on = stratify_on, delta = delta)

  if(pcvpc){
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

    typical_sim$idv <- typical_sim[[idv]]
    typical_sim[["bin"]][typical_sim$a.u.g==0] <- define_bin(typical_sim[typical_sim$a.u.g==0,], stratify_on = stratify_on, delta = delta)
    typical_sim[["bin"]][typical_sim$a.u.g==1] <- define_bin(typical_sim[typical_sim$a.u.g==1,], stratify_on = stratify_on, delta = delta)

    # Median PRED tab

    medpredtab <- typical_sim %>%
      filter(.data$a.u.g == 1) %>%
      group_by(.data$bin) %>%
      summarise(PREDbin = stats::median(.data$value))

    SIMTAB <- SIMTAB %>%
      left_join(medpredtab, by = "bin") %>%
      arrange(.data$ID) %>%
      mutate(value = predcorr(
        Yij = .data$value,
        PREDbin = .data$PREDbin,
        PREDij = rep(typical_sim$value[typical_sim$a.u.g==1], nrep)
      )
      )

    OBSTAB <- OBSTAB %>%
      left_join(medpredtab, by = "bin") %>%
      mutate(value = predcorr(
        Yij = .data$value,
        PREDbin = .data$PREDbin,
        PREDij = filter(
          typical_sim,
          .data$a.u.g == 0,
          .data$name == unique(.data$name)[1]
        )$value[data$evid == 0]
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
