predcorr <- function(Yij, PREDbin, PREDij){
  pcYij <- Yij * PREDbin / PREDij
  pcYij[Yij==0 & PREDij==0] <- 0
  pcYij
}

define_bin <- function(x, stratify_on){
  x$roundidv <- round(x$idv)
  if(is.null(stratify_on)){
    ans <- x$roundidv
  } else {
    ans <- do.call(interaction, list(x[, c("roundidv", stratify_on)], sep = "_"))
  }
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
      carry_out = c("a.u.g", stratify_on),
      Request = Request,
      tad = idv == "tad",
      ... = ...,
      new_omega = NULL, #stochastic simulations
      nrep = nrep # with n replicates
    )
  )

  OBSTAB <- data %>% as_tibble
  OBSTAB$idv <- stochastic_sim[[idv]][stochastic_sim$a.u.g==0 & stochastic_sim$ID == 1]
  OBSTAB <- OBSTAB[OBSTAB$evid == 0, ]
  OBSTAB$bin <- define_bin(OBSTAB, stratify_on = stratify_on)

  stochastic_sim <- bind_rows(stochastic_sim) %>% as_tibble
  SIMTAB <- stochastic_sim[stochastic_sim$a.u.g == 1, ]
  SIMTAB$idv <- SIMTAB[[idv]]
  SIMTAB$bin <- define_bin(SIMTAB, stratify_on = stratify_on)

  if(pcvpc){
    typical_sim <- bind_rows(
      pmap(
        .l = sim_args,
        .f = do_mapbayr_sim,
        x = x,
        carry_out = c("a.u.g", stratify_on),
        Request = Request,
        tad = idv == "tad",
        ... = ...,
        new_omega = "zero_re", # typical simulations
        nrep = NULL # no replicates
      )
    )

    typical_sim$idv <- typical_sim[[idv]]
    typical_sim$bin[typical_sim$a.u.g==0] <- define_bin(typical_sim[typical_sim$a.u.g==0,], stratify_on = stratify_on)
    typical_sim$bin[typical_sim$a.u.g==1] <- define_bin(typical_sim[typical_sim$a.u.g==1,], stratify_on = stratify_on)

    # Median PRED tab

    medpredtab <- typical_sim %>%
      filter(a.u.g == 1) %>%
      group_by(bin) %>%
      summarise(PREDbin = median(DV))

    SIMTAB <- SIMTAB %>%
      left_join(medpredtab, by = "bin") %>%
      arrange(ID) %>%
      mutate(DV = predcorr(
        Yij = DV,
        PREDbin = PREDbin,
        PREDij = rep(typical_sim$DV[typical_sim$a.u.g==1],nrep)
      )
      )
    OBSTAB <- OBSTAB %>%
      left_join(medpredtab, by = "bin") %>%
      mutate(DV = predcorr(
        Yij = DV,
        PREDbin = PREDbin,
        PREDij = typical_sim$DV[typical_sim$a.u.g==0][data$evid==0]
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
    filter(idv < max(vpc_sim$OBSTAB$idv)+2) %>%
    group_by(across(all_of(c("bin", "idv", vpc_sim$stratify_on)))) %>%
    summarise(Q05 = quantile(DV, 0.05),
              Q10 = quantile(DV, 0.10),
              Q25 = quantile(DV, 0.25),
              Q50 = quantile(DV, 0.50),
              Q75 = quantile(DV, 0.75),
              Q90 = quantile(DV, 0.90),
              Q95 = quantile(DV, 0.95)
    )

  p <- dataplot %>%
    ggplot(aes(idv)) +
    geom_line(aes(y = Q50)) +
    geom_ribbon(aes(ymin = Q05, ymax = Q95), alpha = .2, fill = "blue") +
    geom_ribbon(aes(ymin = Q10, ymax = Q90), alpha = .2, fill = "blue") +
    geom_ribbon(aes(ymin = Q25, ymax = Q75), alpha = .2, fill = "blue") +
    ggplot2::geom_text(aes(y = DV, label = ID), data = vpc_sim$OBSTAB) +
    labs(x = idv)

  if(!is.null(stratify_on)){
    p <- p +
      facet_wrap(stratify_on, labeller = label_both)
  }

  p
}
