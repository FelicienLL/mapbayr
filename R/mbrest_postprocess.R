

#' Post process results from optimization
#'
#' @inheritParams mbrest
#' @param opt.value value returned by optimizer
#' @param arg.optim,arg.ofv argument passed to optimizer
#'
#' @return a list of post processing values
#' @export
postprocess <- function(x, data, opt.value, arg.optim, arg.ofv){

  final_eta <- opt.value[eta_names(x)] %>%
    as.double() %>%
    set_names(eta_names(x))

  if(!is.null(opt.value$fevals)){
    if(is.nan(opt.value$fevals)) {
      final_eta <- rep(0, n_eta(x)) %>% set_names(eta_names(x))
      warning("Cannot compute objective function value ; typical value (ETA = 0) returned")
    }
  }

  typical_pred <- x %>%
    data_set(data) %>%
    zero_re() %>%
    mrgsim(end = -1) %>%
    as_tibble() %>%
    pull(.data$DV)

  indiv_pred <- x %>%
    param(final_eta) %>%
    data_set(data) %>%
    zero_re() %>%
    mrgsim(end = -1) %>%
    as_tibble() %>%
    pull(.data$DV)

  mapbay_tab <- data %>%
    mutate(IPRED = indiv_pred, PRED = typical_pred, .after = "DV") %>%
    select(-any_of(x@cmtL)) %>%
    bind_cols(bind_rows(final_eta))

  list(
    final_eta = final_eta,
    mapbay_tab = mapbay_tab
  )

}


#' Build the output of mbrest function
#' @inheritParams mbrest
#' @inheritParams postprocess
#' @param post output of the post.process function
#'
#' @return a mbrests model object
#' @export
output_mbr <- function(x, data, arg.optim, arg.ofv, opt.value, post, output){

  if(!is.null(output)){
    if(output == "df") out <- map_dfr(post, "mapbay_tab")

  } else {
    out <- list(
      model = x,
      data = bind_rows(unname(data)),
      arg.optim = arg.optim,
      arg.ofv = arg.ofv,
      opt.value = map_dfr(opt.value, rownames_to_column, var = "method", .id = "ID"),
      final_eta = map(post, "final_eta"),
      mapbay_tab = map_dfr(post, "mapbay_tab")
    )

    class(out) <- "mbrests"

  }
  return(out)

}
