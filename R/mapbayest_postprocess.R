#' Postprocess mapbayr
#'
#' @name postprocess
#' @return `postprocess.optim()` returns a list with final parameters and `mapbay_tab`. `postprocess.output()` returns a `mapbayests` class object.
#' @inheritParams mapbayest
#' @param opt.value value returned by optimizer
#' @param arg.optim,arg.ofv,arg.ofv.fix,arg.ofv.id argument passed to optimizer
#' @param post,times output of the post.process function
#' @description Functions to generate postprocess after optimization.
NULL
#> NULL



#' Post-process: derive predictions from optimization
#' @rdname postprocess
#' @export
postprocess.optim <- function(x, data, opt.value, arg.ofv, arg.optim, hessian){

  #Final eta
  final_eta <- opt.value[eta_names(x)] %>%
    as.double() %>%
    set_names(eta_names(x))

  #Variance Covariance Matrix
  fp <- function(p){ #obj fun value as function of param
    arg <- arg.ofv
    eta <- p
    names(eta) <- eta_names(x)
    arg$eta <- eta
    do.call(compute_ofv, arg)
  }

  safe_solve <- purrr::safely(solve, otherwise = matrix(NA_real_))

  if(hessian){
  #if(hessian[1] %in% c("optimHess", "nlmixrHess")){
  #  if(hessian[1] == "optimHess"){
  #    hess <- do.call(stats::optimHess, args = list(par = final_eta, fn = fp, control = arg.optim$control))
  #  }
  #  if(hessian[1] == "nlmixrHess"){
  #    hess <- do.call(nlmixr::nlmixrHess, args = list(par = final_eta, fn = fp))
  #  }
    hess <- do.call(stats::optimHess, args = list(par = final_eta, fn = fp, control = arg.optim$control))
    covariance <- unname(2 * safe_solve(hess)$result)
  } else {
    covariance <- matrix(NA_real_)
  }

  #Mapbay Tab
  reserved_capt <- c("DV", "PAR", "MET")
  reserved_names <- names(data)[names(data) %in% c("ID", "time", "cmt", "evid", "amt", "mdv", "addl", "rate", "ss", "ii")]
  other_items <- names(data)[!(names(data) %in% c(reserved_names, mbr_cov_names(x), reserved_capt))]
  captured_items <- (x@capL)[!(x@capL) %in% reserved_capt]

  col_DV <- data$DV
  col_PRED <- x %>%
    zero_re() %>%
    mrgsim_df(data = data, end = -1) %>%
    pull(.data$DV)

  tab <- x %>%
    param(final_eta) %>%
    zero_re() %>%
    mrgsim_df(data = data, end = -1, carry_out = c(reserved_names, mbr_cov_names(x), other_items)) %>%
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
    covariance = covariance,
    mapbay_tab = mapbay_tab
  )

}


#' Post-process: Build the output (mapbayests model object)
#' @rdname postprocess
#' @export
postprocess.output <- function(x, arg.optim, arg.ofv.fix, arg.ofv.id, opt.value, post, output, times){

  if(!is.null(output)){
    if(output == "df") out <- map_dfr(post, "mapbay_tab")

  } else {
    out <- list(
      model = x,
      data = bind_rows(unname(map(arg.ofv.id, "data"))),
      arg.optim = arg.optim,
      arg.ofv.fix = arg.ofv.fix,
      arg.ofv.id = arg.ofv.id,
      opt.value = as.data.frame(map_dfr(opt.value, rownames_to_column, var = "method", .id = "ID")),
      final_eta = map(post, "final_eta"),
      covariance = map(post, "covariance"),
      mapbay_tab = map_dfr(post, "mapbay_tab"),
      information = generate_information(times)
    )

    class(out) <- "mapbayests"

  }
  return(out)

}

generate_information <- function(times){
  times[4] <- Sys.time()
  list(
    start = times[1],
    end = times[4],
    duration = as.double.difftime(times[4]-times[1], units = "secs"),
    details = c(
      preprocessing = as.double.difftime(times[2]-times[1], units = "secs"),
      optimization = as.double.difftime(times[3]-times[2], units = "secs"),
      postprocessing = as.double.difftime(times[4]-times[3], units = "secs")
    ),
    version = c(
      mapbayr = as.character(utils::packageVersion("mapbayr")),
      mrgsolve = as.character(utils::packageVersion("mrgsolve")),
      optimx = as.character(utils::packageVersion("optimx"))
    )
  )
}
