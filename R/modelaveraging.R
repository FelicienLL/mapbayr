#' Average predictions from multiple models
#'
#' @description
#' Model Averaging consists in analyzing the same data with different models
#' and to average their predictions.
#' In order to perform weighted means of clearance predictions, (or
#' concentrations, or any metric of interest), it is necessary to compute
#' the "weight" of each estimation.
#' It is informed by the likelihood of estimation.
#' Two weighting scheme are currently implemented, one based on the log-
#' likelihood ("LL", the default), the other on the Akaike criterion ("AIC").
#' The method was previously described by Uster et al
#' [(Clinical Pharmacology and Therapeutics, 2021)](https://ascpt.onlinelibrary.wiley.com/doi/full/10.1002/cpt.2065).
#'
#' @param ... estimation objects generated from [mapbayest()] to compute weight from it
#' @param output_function a unique function that takes any estimation object and returns a table with controlled variables, dimensions and attributes.
#' @param scheme scheme weight, either "LL" or "AIC"
#' @param estlist a list of estimation objects. Overrides `...`
#' @param list_of_tabs,weights_matrix respectively outputs of the `output_function` and [compute_weights()]
#'
#' @return
#' * [model_averaging()] and [do_model_averaging()]: a data.frame of the same dimensions and attributes as the outputs
#' * [compute_weights()]: a matrix with IDs as rows and estimation weights as columns
#' @export
#'
#' @examples
#' library(magrittr)
#' same_data_and_est <- function(x){
#'   x %>%
#'     adm_rows(ID = 2, time = 0, amt = 100, addl = 3, ii = 24) %>%
#'     obs_rows(ID = 2, time = 96, DV = 1) %>%
#'     adm_rows(ID = 9, time = 0, amt = 200, addl = 3, ii = 24) %>%
#'     obs_rows(ID = 9, time = 96, DV = 2) %>%
#'     mapbayest()
#'   }
#'
#' create_output <- function(x){
#'   x <- as.data.frame(x)
#'   x[x$mdv == 0,c("ID", "time", "DV", "IPRED")]
#'   }
#'
#' mod <- exmodel(1, add_exdata = FALSE)
#'
#' est1 <- mod %>%
#'   mrgsolve::param(TVCL = 2) %>%
#'   same_data_and_est()
#'
#' est2 <- mod %>%
#'   mrgsolve::param(TVCL = 10) %>%
#'   same_data_and_est()
#'
#' model_averaging(
#'   est1,
#'   est2,
#'   output_function = create_output
#'   )
#'
#' W <- compute_weights(CL2 = est1, CL10 = est2)
#' compute_weights(estlist = list(A = est1, B = est2, est1))
#'
#' do_model_averaging(
#'   list_of_tabs = list(create_output(est1), create_output(est2)),
#'   weights_matrix = W
#'   )
#'
model_averaging <- function(...,
                            output_function = as.data.frame,
                            scheme = c("LL", "AIC"),
                            estlist = NULL){

  estlist <- process_est_objects(... = ..., estlist = estlist)

  weights <- compute_weights(scheme = scheme,
                             estlist = estlist)

  tabs <- map(estlist, .f = output_function)

  do_model_averaging(tabs, weights)
}

process_est_objects <- function(..., estlist = NULL){
  dots <- list(...)

  if(is.null(estlist)){
    estlist <- dots
  } else {
    if(length(dots) > 0){
      message("estlist not NULL, arguments passed to `...` will be ignored")
    }
  }

  if(!all(sapply(estlist, inherits, "mapbayests"))){
    add_msg <- NULL
    if(all(sapply(estlist[[1]], inherits, "mapbayests"))){
      add_msg <- "\n Did you forget to call explicitely `estlist = `?"
    }
    stop("All objects passed to `compute_weights()` must be `mapbayests` class object", add_msg)
  }

  IDs <- lapply(estlist, function(x){x$opt.value$ID})

  if(length(unique(IDs)) != 1){
    stop("Subject IDs are not the same between estimation objects")
  }

  return(estlist)
}

get_LL <- function(x, LL = TRUE){
  opt <- x$opt.value
  ans <- matrix(opt$value, dimnames = list(opt$ID, NULL))
  if(LL){
    ans <- exp(-0.5 * ans)
  }
  ans
}

get_AIC <- function(x){
  OFV <- get_LL(x, LL = FALSE)
  k <- sum(grepl("ETA", names(x$opt.value)))
  exp(-0.5 * OFV - k)
}

#' @rdname model_averaging
#' @export
compute_weights <- function(...,
                            scheme = c("LL", "AIC"),
                            estlist = NULL){

  estlist <- process_est_objects(... = ..., estlist = estlist)

  scheme <- scheme[1]

  scheme_fn <- switch(scheme,
                      LL = get_LL,
                      AIC = get_AIC)

  values <- lapply(estlist, scheme_fn) #no sapply to keep rownames
  values <- do.call(cbind, values)
  colnames(values) <- names(estlist)
  values / rowSums(values)

}

apply_weights <- function(itabs, #list of tabs, one per model, one ID per tab
                          iweights #a vector of weights, one per model, for 1 ID
){

  first_tab <- itabs[[1]]

  sapply(itabs, are_comparable, first_tab)

  numeric_variables <- sapply(first_tab, is.numeric)

  list_weighted_itabs <- mapply(
    FUN = `*`,
    lapply(itabs, `[`, numeric_variables),
    iweights,
    SIMPLIFY = FALSE
  )

  first_tab[numeric_variables] <- Reduce(
    f = `+`,
    x = list_weighted_itabs
  )

  first_tab
}

#' @rdname model_averaging
#' @export
do_model_averaging <- function(list_of_tabs, weights_matrix){

  # Need to work to the individual level
  # Transpose list of tables to a list of (individual) list of tables
  tabs <- list_of_tabs %>%
    map(split_mapbayr_data) %>%
    purrr::list_transpose(simplify = FALSE)

  # Turn matrix in to a list of individual vectors
  weights <- asplit(weights_matrix, 1)

  # Apply weights to each individual
  map2(
    .x = tabs,
    .y = weights,
    .f = apply_weights
    ) %>%
    bind_rows()
}

are_comparable <- function(a, b){
  stopifnot(all.equal(
    attributes(a),
    attributes(b)
    ))

  stopifnot(all.equal(
    sapply(a, class),
    sapply(b, class)
    ))

  if(inherits(a, "data.frame")){
    stopifnot(all.equal(
      a[!sapply(a, is.numeric)],
      b[!sapply(b, is.numeric)]
    ))
  }

}
