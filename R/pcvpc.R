predcorr <- function(Yij, PREDbin, PREDij){
  pcYij <- Yij * PREDbin / PREDij
  pcYij[Yij==0 & PREDij==0] <- 0
  pcYij
}

define_bin <- function(x, stratify_on){
  x$roundtad <- round(x$tad)
  if(is.null(stratify_on)){
    ans <- x$roundtad
  } else {
    ans <- do.call(interaction, x[, c("roundtad", stratify_on)])
  }
  return(ans)
}

vpc_sim <- function(x,
                    data = NULL,
                    nrep = 20,
                    pcvpc = TRUE,
                    stratify_on = NULL, #do not work yet
                    end = max(data$time)*1.2,
                    ...){

  if(is.null(data)){
    data <- x@args$data
  }
  x@args$data <- NULL #empty the data slot in model object

  stochastic_sim <- mrgsim(x, data = replicate_data(data, nrep = nrep),
                           tad = TRUE, obsaug = TRUE, end = end,
                           carry_out = c("a.u.g", stratify_on), output = "df",
                           ... = ...)
  OBSTAB <- data
  OBSTAB$tad <- stochastic_sim$tad[stochastic_sim$a.u.g==0][seq_len(nrow(data))]
  OBSTAB <- OBSTAB[OBSTAB$evid == 0, ]
  OBSTAB$bin <- define_bin(OBSTAB, stratify_on = stratify_on)

  SIMTAB <- stochastic_sim[stochastic_sim$a.u.g == 1, ]
  SIMTAB$bin <- define_bin(SIMTAB, stratify_on = stratify_on)

  if(pcvpc){
    typical_sim <- mrgsim(zero_re(x), data = data,
                          tad = TRUE, obsaug = TRUE, end = end,
                          carry_out = c("a.u.g", stratify_on), output = "df", ... = ...)

    typical_sim$bin <- define_bin(typical_sim, stratify_on = stratify_on)

    # Median PRED tab

    medpredtab <- typical_sim %>%
      filter(a.u.g == 1) %>%
      group_by(bin) %>%
      summarise(PREDbin = median(DV))

    SIMTAB <- SIMTAB %>%
      left_join(medpredtab, by = "bin") %>%
      mutate(DV = predcorr(
        Yij = DV,
        PREDbin = PREDbin,
        PREDij = rep(typical_sim$DV[typical_sim$a.u.g==1], nrep)
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
    OBSTAB = OBSTAB
  )

}

vpc_plot <- function(vpc_sim){
  vpc_sim$SIMTAB %>%
    filter(tad < max(vpc_sim$OBSTAB$tad)+2) %>%
    group_by(bin, tad) %>%
    summarise(Q05 = quantile(DV, 0.05),
              Q10 = quantile(DV, 0.10),
              Q25 = quantile(DV, 0.25),
              Q50 = quantile(DV, 0.50),
              Q75 = quantile(DV, 0.75),
              Q90 = quantile(DV, 0.90),
              Q95 = quantile(DV, 0.95)
    ) %>%
    ggplot(aes(tad)) +
    geom_line(aes(y = Q50)) +
    geom_ribbon(aes(ymin = Q05, ymax = Q95), alpha = .2, fill = "blue") +
    geom_ribbon(aes(ymin = Q10, ymax = Q90), alpha = .2, fill = "blue") +
    geom_ribbon(aes(ymin = Q25, ymax = Q75), alpha = .2, fill = "blue") +
    geom_point(aes(y = DV), data = vpc_sim$OBSTAB)
}

