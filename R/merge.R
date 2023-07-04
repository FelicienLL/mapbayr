merge_datadf_etamatrix <- function(data_df, eta_matrix){
  #data_df = a data set
  #eta_matrix = a matrix of eta (no ID column)
  left_join(x = data_df, y = mutate(as.data.frame(eta_matrix), ID = as.double(rownames(eta_matrix))), by = "ID")
}

merge_datamatrix_etamatrix <- function(data_matrix, eta_matrix){
  ta <- table(data_matrix[,'ID'])
  unique_ids <- names(ta)
  nrow_per_id <- as.double(ta)

  if(length(unique_ids) != nrow(eta_matrix)){
    stop("Number of subjects in data is not the number of subjects in 'ETA' matrix")
  }

  if(is.null(rownames(eta_matrix))){
    rownames(eta_matrix) <- unique_ids
  }

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

replicate_data <- function(data, nrep){
  repeated_data <- sapply(data, rep, nrep)

  new_IDs <- interaction(
    repeated_data[,"ID"],
    rep(seq_len(nrep), each = nrow(data))
  )

  repeated_data[,"ID"] <- as.numeric(new_IDs)
  repeated_data
}
