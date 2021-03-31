#' Postprocess mapbayr
#'
#' @name postprocess
#' @inheritParams mbrest
#' @param opt.value value returned by optimizer
#' @param arg.optim,arg.ofv.fix,arg.ofv.id argument passed to optimizer
#' @param post output of the post.process function
#' @description Functions to generate postprocess after optimization.
NULL
#> NULL



#' Post-process: derive predictions from optimization
#' @rdname postprocess
#' @export
postprocess.optim <- function(x, data, opt.value){

  final_eta <- opt.value[eta_names(x)] %>%
    as.double() %>%
    set_names(eta_names(x))

  if(!is.null(opt.value$fevals)){
    if(is.nan(opt.value$fevals)) {
      final_eta <- rep(0, n_eta(x)) %>% set_names(eta_names(x))
      warning("Cannot compute objective function value ; typical value (ETA = 0) returned")
    }
  }

  reserved_capt <- c("DV", "PAR", "MET")
  reserved_names <- names(data)[names(data) %in% c("ID", "time", "cmt", "evid", "amt", "mdv", "addl", "rate", "ss", "ii")]
  other_items <- names(data)[!(names(data) %in% c(reserved_names, mbr_cov_names(x), reserved_capt))]
  captured_items <- (x@capL)[!(x@capL) %in% reserved_capt]

  col_DV <- data$DV
  col_PRED <- x %>%
    data_set(data) %>%
    zero_re() %>%
    mrgsim_df(end = -1) %>%
    pull(.data$DV)

  tab <- x %>%
    param(final_eta) %>%
    data_set(data) %>%
    zero_re() %>%
    mrgsim_df(end = -1, carry_out = c(reserved_names, mbr_cov_names(x), other_items)) %>%
    rename(IPRED = .data$DV) %>%
    select(-any_of(x@cmtL)) %>%
    mutate(PRED = col_PRED,
           DV = col_DV)

  missing_cov <- mbr_cov_refvalues(x)[!names(mbr_cov_refvalues(x)) %in% names(data)]

  mapbay_tab <- tab %>%
    bind_cols(bind_rows(c(missing_cov, final_eta))) %>%
    relocate(reserved_names, "DV", "IPRED", "PRED", any_of(reserved_capt), captured_items, mbr_cov_names(x), other_items, eta_names(x))

  list(
    final_eta = final_eta,
    mapbay_tab = mapbay_tab
  )

}


#' Post-process: Build the output (mbrests model object)
#' @rdname postprocess
#' @export
postprocess.output <- function(x, arg.optim, arg.ofv.fix, arg.ofv.id, opt.value, post, output){

  if(!is.null(output)){
    if(output == "df") out <- map_dfr(post, "mapbay_tab")

  } else {
    out <- list(
      model = x,
      data = bind_rows(unname(map(arg.ofv.id, "data"))),
      arg.optim = arg.optim,
      arg.ofv.fix = arg.ofv.fix,
      arg.ofv.id = arg.ofv.id,
      opt.value = map_dfr(opt.value, rownames_to_column, var = "method", .id = "ID"),
      final_eta = map(post, "final_eta"),
      mapbay_tab = map_dfr(post, "mapbay_tab")
    )

    class(out) <- "mbrests"

  }
  return(out)

}
