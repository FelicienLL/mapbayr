#' Postprocess mapbayr
#'
#' @name postprocess
#' @return `postprocess.optim()` returns a list with final parameters and `mapbay_tab`. `postprocess.output()` returns a `mapbayests` class object.
#' @inheritParams mapbayest
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

  out <- list()

  #FINAL_ETA

  out$final_eta <- opt.value[eta_names(x)] %>%
    as.double() %>%
    set_names(eta_names(x))

  #HESSIAN (FIM and SE)
  hess <- attr(opt.value, "details")[1,]$nhatend

  if(!any(is.na(hess))){
    out$fisher_information_matrix <- hess
    out$standard_error <- sqrt(diag(solve(out$fisher_information_matrix)))
    names(out$standard_error) <- eta_names(x)
  }

  #MAPBAY_TAB

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
    param(out$final_eta) %>%
    data_set(data) %>%
    zero_re() %>%
    mrgsim_df(end = -1, carry_out = c(reserved_names, mbr_cov_names(x), other_items)) %>%
    rename(IPRED = .data$DV) %>%
    select(-any_of(x@cmtL)) %>%
    mutate(PRED = col_PRED,
           DV = col_DV)

  missing_cov <- mbr_cov_refvalues(x)[!names(mbr_cov_refvalues(x)) %in% names(data)]

  out$mapbay_tab <- tab %>%
    bind_cols(bind_rows(c(missing_cov, out$final_eta))) %>%
    relocate(reserved_names, "DV", "IPRED", "PRED", any_of(reserved_capt), captured_items, mbr_cov_names(x), other_items, eta_names(x))

  return(out)

}


#' Post-process: Build the output (mapbayests model object)
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
      final_eta = map(post, "final_eta")
    )

    if(arg.optim$hessian){
      out$fisher_information_matrix <- map(post, "fisher_information_matrix")
      out$standard_error <- map(post, "standard_error")
    }

    out$mapbay_tab <- map_dfr(post, "mapbay_tab")

    class(out) <- "mapbayests"

  }

  return(out)

}
