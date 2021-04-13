#' Get content from object
#'
#' @name get_x
#' @param x object to get content from
#' @param ... passed along
#'
#' @description Helpful functions to get content from a `mrgmod` object (i.e. data) or from a `mapbayests` object (data, eta, param).
NULL
#> NULL

#' @rdname get_x
#' @export
get_data <- function(x, ...)UseMethod("get_data")

#' Return data from a mrgmod
#'
#' @param x model object
#' @param ... not used
#'
#' @method get_data mrgmod
#' @return a tibble
#' @export
get_data.mrgmod <- function(x, ...){
  as_tibble(x@args$data)
}

#' Return data from a mapbayests
#'
#' @param x mapbayests object
#' @param ... not used
#'
#' @method get_data mapbayests
#' @return a tibble
#' @export
get_data.mapbayests <- function(x, ...){
  as_tibble(x$data)
}

#' @rdname get_x
#' @export
get_param <- function(x, ...) UseMethod("get_param")

#' Return a posteriori param from a mapbayests
#'
#' @param x mapbayests object
#' @param ... passed along
#' @param output either a list ("list"), a data.frame ("df") or a vector of numeric ("num"). Default to "num" if only one ID
#'
#' @method get_param mapbayests
#' @return a tibble
#' @export
get_param.mapbayests <- function(x, ..., output = NULL){
  #Variable selection

  selected <- unique(unlist(list(...)))
  captured <- x$model@capL
  ok_captured <- captured[!captured %in% c("DV", "PAR", "MET")]

  if(length(selected) == 0){
    ok_names <- ok_captured
  } else {
    ok_names <- selected[selected %in% ok_captured]
  }

  #output selection
  oneID <- (length(x$arg.ofv.id) == 1)

  if(is.null(output)){
    if(oneID){
      .out <- "num"
    } else {
      .out <- "df"
    }
  } else {
    okout <- c("num", "list", "df")
    if(!output %in% okout) stop('Allowed output are: ', paste(okout, collapse = ", "), ".")
    .out <- output[1]
  }

  if(!oneID & .out == "num") stop("Multiple ID, cannot coerce list to a vector of numeric.")

  par_tab <- x$mapbay_tab %>%
    select(.data$ID, dplyr::any_of(ok_names)) %>%
    group_by(.data$ID) %>%
    dplyr::slice(1) %>%
    dplyr::ungroup()

  par <- switch (.out,
                 "num" = unlist(par_tab),
                 "list" = map(split_mapbayr_data(par_tab), unlist),
                 "df"= par_tab
  )

  return(par)
}
