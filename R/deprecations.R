#' Deprecated functions
#'
#' @name deprecations
#' @param ... passed to the corresponding function
#' @details
#' * mbrest() is now [mapbayest()]
#' * adm_lines() is now [adm_rows()]
#' * obs_lines() is now [obs_rows()]
NULL

#' @export
#' @rdname deprecations
mbrest <- function(...){
  .Deprecated("mapbayest")
  mapbayest(...)
}

#' @export
#' @rdname deprecations
adm_lines <- function(...){
  .Deprecated("adm_rows")
  adm_rows(...)
}

#' @export
#' @rdname deprecations
obs_lines <- function(...){
  .Deprecated("obs_rows")
  obs_rows(...)
}
