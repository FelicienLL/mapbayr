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
#' The method was previously described by Uster et al (2021) \doi{10.1002/cpt.2065}.
#'
#' @param ... estimation objects generated with [mapbayest()], from which the weights will be computed
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
#' library(mrgsolve)
#'
#' # Three different models: A, B, and C.
#' modA <- exmodel(1, add_exdata = FALSE)
#' modB <- param(modA, TVCL = 2, TVVC = 30)
#' modC <- param(modA, TVCL = 10)
#'
#' # A common dataset that has 2 patients (ID 2 & 9)
#' data <- adm_rows(ID = 2, time = 0, amt = 200, addl = 3, ii = 24, cmt = 1) %>%
#'     obs_rows(ID = 2, time = 84, DV = 1.5, cmt = 2) %>%
#'     adm_rows(ID = 9, time = 0, amt = 100, addl = 3, ii = 24, cmt = 1) %>%
#'     obs_rows(ID = 9, time = 96, DV = 1, cmt = 2)
#'
#' # Three different estimation objects: A, B and C.
#' estA <- mapbayest(modA, data)
#' as.data.frame(estA)
#' plot(estA) # Fit is pretty good
#'
#' estB <- mapbayest(modB, data)
#' as.data.frame(estB)
#' plot(estB) # Excellent fit
#'
#' estC <- mapbayest(modC, data)
#' as.data.frame(estC)
#' plot(estC) # Fit is worst
#'
#' # Model averaging
#' model_averaging(A = estA, B = estB, C = estC)
#' # Weighted average of the table returned by as.data.frame(est))
#'
#' # Internally, it first computes the "weight" of each model such as:
#' W <- compute_weights(A = estA, B = estB, C = estC)
#'
#' # Then multiply the prediction table with each weight such as:
#' do_model_averaging(
#'   list_of_tabs = list(
#'     A = as.data.frame(estA),
#'     B = as.data.frame(estB),
#'     C = as.data.frame(estC)
#'     ),
#'   weights_matrix = W
#'   )
#'
#' # If you do not want to perform an average of the full table, you can specify
#' # a function that takes the estimation object as an input and returns
#' # value(s) of interest: a single prediction, a clearance value, a full
#' # table of augmented predictions... as long as the structure of the final
#' # object is the same whatever the model.
#'
#' reframe <- function(est){
#'   # From any estimation object, return a table with ID, time and predictions
#'   as.data.frame(est)[,c("ID", "time", "DV", "IPRED")]
#' }
#'
#' model_averaging(A = estA, B = estB, C = estC, output_function = reframe)
#'
#' # Make a plot that compares predictions
#' List_aug_tab <- lapply(
#'   X = list(A = estA, B = estB, C = estC),
#'   FUN = \(x) augment(x)$aug_tab
#' )
#' List_aug_tab$.AVERAGE <- do_model_averaging(List_aug_tab, W)
#'
#' mapbayr_plot(
#'  aug_tab = dplyr::bind_rows(List_aug_tab, .id = "MODEL"),
#'  obs_tab = data,
#'  PREDICTION = "IPRED",
#'  MODEL_color = c(.AVERAGE = "black")
#' )
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
  list_weighted_itabs <- mapply(
    FUN = `*`,
    itabs,
    iweights,
    SIMPLIFY = FALSE
  )

  final_tab <- Reduce(
    f = `+`,
    x = list_weighted_itabs
  )

  round(final_tab, 12) # Fix floating point errors.
}

#' @rdname model_averaging
#' @export
do_model_averaging <- function(list_of_tabs, weights_matrix){

  first_tab <- list_of_tabs[[1]]
  # Ensure comparison between tables
  sapply(list_of_tabs, are_comparable, first_tab)

  # Will apply weighted mean on numeric variables only
  numeric_variables <- sapply(first_tab, is.numeric)

  # Need to work to the individual level
  # Transpose list of tables to a list of (individual) list of tables
  tabs <- list_of_tabs %>%
    map(split_mapbayr_data) %>%
    map(~ map(.x, `[`, numeric_variables)) %>% #cannot do before split in case ID is not numeric
    purrr::list_transpose(simplify = FALSE)

  # Turn matrix in to a list of individual vectors
  weights <- asplit(weights_matrix, 1)

  # Apply weights to each individual
  ans <- map2(
    .x = tabs,
    .y = weights,
    .f = apply_weights
    ) %>%
    bind_rows()

  first_tab[numeric_variables] <- ans

  first_tab

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
