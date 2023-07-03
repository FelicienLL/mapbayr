do_mapbayr_sim <- function(
    x,
    data,
    carry_out = character(0),
    obsaug = FALSE,
    recsort = 3,
    output = "df",
    Request = character(0),
    ...,
    eta = NULL,
    nrep = NULL,
    new_omega = omat(zero_re(x)), # NULL = use in the model, default to zero_re. Used only if nrep is non-NULL
    new_sigma = smat(zero_re(x))  # NULL = use in the model, default to zero_re. Used only if nrep is non-NULL
){

  k <- 1
  if(has_eta_param(x)) k <- 0.5

  nID <- length(unique(data$ID))

  if(!is.null(new_omega)){
    x <- mrgsolve::collapse_omega(x)
    x <- mrgsolve::omat(x, new_omega)
  }

  if(!is.null(new_sigma)){
    x <- mrgsolve::collapse_sigma(x)
    x <- mrgsolve::smat(x, new_sigma)
  }

  data_to_sim <- data

  if(!is.null(nrep)){
    data_to_sim <- replicate_data(data_to_sim, nrep)
  }

  etasrc <- "omega"
  if(!is.null(eta)){
    if(!is.matrix(eta)){
      eta <- matrix(
        data = eta, nrow = 1,
        dimnames = list(unique(data$ID), names(eta))
      )
    }
    stopifnot(nrow(eta) == nID)

    eta_matrix <- eta

    if(!is.null(nrep)){
      eta_matrix <- matrix(
        data = rep(eta, nrep),
        ncol = ncol(eta),
        byrow = TRUE,
      )
      # Replicated individual point estimates
      # nrow = n(original ID) x n(replicates)

      if(any(omat(x, make = TRUE) != 0)){ # Add "noise" around point estimates
        eta_sim_matrix <- mvgauss(
          mat = omat(x, make = TRUE),
          n = nrep * nID
        ) # nrow = n(original ID) x n(replicates)

        eta_matrix <- eta_matrix + eta_sim_matrix
      }
    }

    eta_matrix <- rename_as_eta(eta_matrix) * k # ETA(1)/2 + ETA1/2
    if(is.matrix(data_to_sim)){
      data_to_sim <- merge_datamatrix_etamatrix(data_to_sim, eta_matrix)
    } else {
      data_to_sim <- dataeta(data_to_sim, eta_matrix)
    }

    etasrc <- "data.all"
  }

  ans <- mrgsim(
    x = x,
    data = data_to_sim,
    carry_out = carry_out,
    obsaug = obsaug,
    recsort = recsort,
    output = output,
    Request = Request,
    ... = ...,
    etasrc = etasrc
  )

  ans
}

has_eta_param <- function(x){
  all(
    make_eta_names(n = length(odiag(x))) %in% grep('ETA\\d+', names(x@param), value = TRUE)
  )
}

replicate_data <- function(data, nrep){
  repeated_data <- sapply(data, rep, nrep)

  new_IDs <- interaction(
    repeated_data[,"ID"],
    rep(seq_len(nrep), each = nrow(data))
  )

  repeated_data[,"ID"] <- as.numeric(new_IDs)
  repeated_data
}

merge_datamatrix_etamatrix <- function(data_matrix, eta_matrix){
  unique_ids <- unique(data_matrix[,"ID"])
  nrow_per_id <- tabulate(data_matrix[,"ID"])

  eta_matrix_big <- mapply(function(id, rowcount){
    matrix(
      data = rep(eta_matrix[id,], rowcount),
      ncol = ncol(eta_matrix),
      byrow = TRUE
    )},
    id = unique_ids,
    rowcount = nrow_per_id,
    SIMPLIFY = FALSE
  ) %>%
    do.call(what = rbind) %>%
    rename_as_eta()

  cbind(data_matrix, eta_matrix_big)
}
