sirest <- function(x, data, nM = 1e5, nm = 1e4, .seed){

  # To do :
  # - one ID only
  # - out preprocess
  # - seed


  argofv <- c(preprocess.ofv.fix(x = x), preprocess.ofv.id(x = x, iddata = data))
  omega <- omat(x, make = TRUE)
  n_eta <- n_eta(x)
  n_conc <- length(argofv$idDV)

  # Adapted from Dosne et al, J Pharmacokinet Pharmacodyn, 2016
  # Supplementary material 1
  # Available online at https://link.springer.com/article/10.1007/s10928-016-9487-8

  # --- Step 1: Sampling

  simmat <- mvtnorm::rmvnorm(n = nM,
                             mean = rep(0, n_eta),
                             sigma = omega)

  simmat <- rename_as_eta(simmat)

  # --- Step 2: Importance weighting
  # --- 2.a : Compute the objective function for each sampled vector of ETA.

  # Replicate the data set nM times with different ETA
  dattosim <- merge_data_etamatrix(data = data, matrix = simmat)

  # Compute vectors of predictions (preds) and matrix of derivatives (hs)
  preds <- f(qmod = argofv$qmod, data = dattosim)
  hs <- h(preds, argofv$idcmt, all_cmt = argofv$all_cmt)

  # Matrix multiplication (H %*% sigma %*% t(H)) is too expansive with such a long matrix
  # Thus, I chunk the matrix into pieces and merge them afterwards
  # Quick benchmark showed that chunks of length 50 were satisfying
  # Obviously there is a room for performance optimization here: but for now it works
  chunkedvar <- function(H, nchunk = 50){
    res <- lapply(chunk(seq_len(nrow(H)), nchunk), function(i){
      diag(hs[i, ] %*% argofv$sigma %*% t(hs[i,]))
    })
    unlist(res)
  }

  # Compute the vector of variances
  vars <- chunkedvar(hs)

  # Prepare a single matrix with etas, preds and variance to compute the objective function values
  index_eta <- seq_len(n_eta)
  index_pred <- seq(n_eta+1, n_eta+n_conc)
  index_var <- seq(n_eta+n_conc+1, n_eta+n_conc*2)

  vec_ofv <- function(m){
    sum(log(m[index_var]) + (argofv$idDV - m[index_pred])^2/(m[index_var])) + diag(matrix(m[index_eta], nrow = 1) %*% argofv$omega_inv %*% matrix(m[index_eta], ncol = 1))
  }

  ofvs <- apply(
    X = cbind(
      simmat,
      matrix(preds, nrow = nM, byrow = TRUE),
      matrix(vars, nrow = nM, byrow = TRUE)
    ),
    MARGIN = 1,
    FUN = vec_ofv
  )

  # --- 2.b : Computation of the weights per se

  ml <- which.min(ofvs)

  dOFVi <- ofvs - ofvs[ml]
  p.prime.i <- exp(-0.5 * dOFVi)
  PDFi <- mvtnorm::dmvnorm(x = simmat, mean = rep(0, n_eta), sigma = omega)
  PDFml <- PDFi[ml]
  relPDF <- PDFi/PDFml
  IRi <- p.prime.i / relPDF
  prob_resampling <- IRi / sum(IRi)

  # --- Step 3: Resampling

  resampling_indices <- sample(x = seq_len(nM),
                               size = nm,
                               replace = TRUE,
                               prob = prob_resampling)

  simmat[resampling_indices,]
}

# thanks @verbamour https://stackoverflow.com/questions/3318333/split-a-vector-into-chunks
chunk <- function(x, n) {
  mapply(function(a, b) (x[a:b]), seq.int(from=1, to=length(x), by=n), pmin(seq.int(from=1, to=length(x), by=n)+(n-1), length(x)), SIMPLIFY=FALSE)
}

merge_data_etamatrix <- function(data, matrix){
  data$ID <- NULL
  merge(data, cbind(ID = seq_len(nrow(matrix)), matrix))
}

postprocess.sir <- function(x, data, sir.value){
  col_PRED <- mrgsim_df(zero_re(x),
                        data = data,
                        end = -1) %>%
    select(PRED = DV)

  col_IPRED <- mrgsim_df(zero_re(x),
                         data = merge_data_etamatrix(data, sir.value),
                         end = -1) %>%
    group_by(time) %>%
    summarise(IPRED_LOW = quantile(DV, 0.025),
              IPRED = quantile(DV, 0.50),
              IPRED_UP = quantile(DV, 0.975)
    ) %>%
    select(IPRED_LOW, IPRED, IPRED_UP)

  sir_tab <- bind_cols(data, col_PRED, col_IPRED)

  print(sir_tab)

  return(invisible(list(
    model = x,
    data = data,
    final_eta = sir.value,
    sir_tab = sir_tab
  )))
}

plot_sir <- function(x){
  .delta = 0.25

  PREDs <- mrgsim_df(zero_re(x$model, "sigma"),
                     data = merge_data_etamatrix(x$data, mrgsolve::mvgauss(omat(x$model, make = TRUE), n = nrow(x$final_eta))),
                     obsaug = TRUE,
                     delta = .delta,
                     obsonly = TRUE,
                     recsort = 3) %>%
    group_by(.data$time) %>%
    summarise(LOW = quantile(DV, 0.025),
              VAL = quantile(DV, 0.50),
              UP = quantile(DV, 0.975)
    )

  IPREDs <- mrgsim_df(zero_re(x$model),
                      data = merge_data_etamatrix(x$data, x$final_eta),
                      obsaug = TRUE,
                      delta = .delta,
                      obsonly = TRUE,
                      recsort = 3) %>%
    group_by(time) %>%
    summarise(LOW = quantile(DV, 0.025),
              VAL = quantile(DV, 0.50),
              UP = quantile(DV, 0.975)
    )

  list(PRED = PREDs,
       IPRED = IPREDs) %>%
    bind_rows(.id = "PREDICTION") %>%
    ggplot(aes(x = time)) +
    geom_ribbon(aes(ymin = LOW, ymax = UP, fill = PREDICTION, col = PREDICTION), alpha = .3) +
    geom_line(aes(y = VAL, col = PREDICTION)) +
    geom_point(aes(y = DV), data = filter(x$data, evid == 0), size = 3) +
    scale_color_manual(values= c(IPRED = "black", PRED = "deepskyblue1")) +
    scale_linetype_manual(values= c(IPRED = 1, PRED = 2)) +
    scale_fill_manual(values= c(IPRED = "black", PRED = "deepskyblue1"))

}

hist_sir <- function(x, post_as_density = FALSE){
  minlow <- min(x)
  maxup <- max(x)
  xvalues <- seq(minlow - 0.01, maxup + 0.01, 0.05)

  density_tab <- sapply(X = sqrt(c(0.3, 0.2, 0.1)),
                        FUN = dnorm,
                        x = xvalues, mean = 0) %>%
    rename_as_eta() %>%
    cbind(x = xvalues) %>%
    as_tibble() %>%
    pivot_longer(-x)

  sir_tab <- as_tibble(sirmat1) %>%
    pivot_longer(everything())

  p1 <-  ggplot() +
    facet_wrap("name") +
    geom_area(aes(x = .data$x, y = .data$value), data = density_tab, fill = "skyblue", alpha = .3) +
    geom_line(aes(x = .data$x, y = .data$value), data = density_tab) +
    theme_bw() +
    theme(strip.background = element_rect(fill = "white"))+
    scale_y_continuous(name = NULL, labels = NULL)+
    scale_x_continuous(name = NULL, n.breaks = 10)

  if(post_as_density){
    p1 <- p1 +
      ggplot2::geom_density(aes(x = .data$value, y = .data$..density..), data = sir_tab, alpha = .8, col = "black", fill = 'grey')
  } else {
    p1 <- p1 +
      geom_histogram(aes(x = .data$value, y = .data$..density..), data = sir_tab, alpha = .8, col = "black", fill = 'grey', bins = 30)
  }
  return(p1)
}
