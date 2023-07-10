infer_tgrid <- function(data, start = NULL, end = NULL, delta = NULL){
  if(is.null(start)){
    start <- min(data$time)
  }

  if(is.null(end)){
    tmax <- max(data$time)
    end <- tmax + (tmax - start) * 0.2
  }

  if(start == 0 & end == 0){
    end <- 24
  }

  if(is.null(delta)){
    delta <- 10^(floor(log10(abs((end - start)/200))))
  }

  mrgsolve::tgrid(start = start, end = end, delta = delta)
}

length1 <- function(x){
  if(length(x) == 0) {
    list(x)
  } else {
    x
  }
}

ci2q <- function(ci) (1-(ci/100))/2

znorm <- function(ci){
  stopifnot(is.numeric(ci), ci > 0, ci < 100)
  stats::qnorm(1-ci2q(ci))
}

stepeta <- function(eta, i, step = 1e-8){
  if(i == "REF") return(eta)
  eta[i] <- eta[i] + log(1 + step)
  eta
}

prepare_augment <- function(data_list,
                            eta_list,
                            cov_list = NULL, # List of posterior covariance matrices
                            ci_delta = FALSE,
                            start = NULL, end = NULL, delta = NULL){
  args <- expand_grid(
    type = c("IPRED", "PRED"), # "type" first, then the table
    tibble::tibble(
      ORIGID = as.numeric(names(data_list)),
      data = data_list,
      tgrid = mapply(
        FUN = infer_tgrid,
        data = data_list,
        start = length1(start),
        end = length1(end),
        delta = length1(delta)
      )
    )
  )

  args$eta <- c(
    eta_list, # for IPRED
    map(eta_list, `*`, 0) # for PRED
    )

  if(ci_delta){
    args <- expand_grid(
      args,
      ci_delta = c("REF", names(eta[[1]]))
    ) %>%
      mutate(eta = map2(eta, ci_delta, stepeta))
  } else {
    if(!is.null(cov_list)){
      args$new_omega <- c(
        cov_list, # for IPRED
        replicate(nrow(args)/2, NULL, simplify = FALSE) # for PRED
      )
    }
  }

  args
}

reframe_augment <- function(tab,
                            cov_list = NULL, # List of posterior covariance matrices
                            iiv_mat = NULL, # Original matrix of IIV
                            ci_width = 90){ #90%

  tab <- pivot_sims(tab)

  if(all(tab$ORIGID == tab$ID)){
    tab$ORIGID <- NULL
    if(!is.null(tab[["ci_delta"]])){
      reftab <- tab[tab$ci_delta == "REF",]
      etatab <- tab[tab$ci_delta != "REF",]

      etanames <- unique(etatab$ci_delta)

      eta_values <- etatab %>%
        split(~ ID + type) %>%
        map(function(d){
          m <- matrix(
            data = d$value,
            ncol = length(etanames))
          rename_as_eta(m)
        })

      ref_values <- reftab %>%
        split(~ ID + type) %>%
        map(function(d){
          m <- matrix(
            data = rep(d$value, length(etanames)),
            ncol = length(etanames))
        })

      jacobians <- map2(eta_values, ref_values, .f = function(x,y)(x-y)/log(1+1e-8))

      paramvarcov <- c(cov_list, replicate(length(cov_list), iiv_mat, simplify = FALSE))

      errors <- mapply(
        FUN = function(.x, .y) znorm(ci_width) * sqrt(diag(.x %*% .y %*% t(.x))),
        jacobians,
        paramvarcov,
        SIMPLIFY = FALSE
        )

      errors <- unlist(errors, use.names = FALSE)

      tab <- reftab %>%
        select(-"ci_delta") %>%
        mutate(
          value_low = .data$value - errors,
          value_up = .data$value + errors
        )
    }
  } else {
    tab <- tab %>%
      group_by(.data$ORIGID, .data$name, .data$type, .data$time) %>%
      summarise(
        value_low = stats::quantile(.data$value, ci2q(ci_width)),
        value_up = stats::quantile(.data$value, 1 - ci2q(ci_width)),
        value = stats::quantile(.data$value, 0.50), # in last position
        .groups = "drop"
      ) %>%
      rename(ID = "ORIGID")
  }

  tab %>%
    arrange(.data$ID, .data$time, .data$name, .data$type)
}

pivot_sims <- function(data, variables = c("DV", "PAR", "MET")){
  pivot_longer(
    data = data,
    cols = any_of(variables),
    names_transform = list(name = ~ factor(.x, levels = variables))
  )
}
