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
get_eta <- function(x, ...) UseMethod("get_eta")


#' Return eta from a mapbayests
#'
#' @param x mapbayests object
#' @param ... not used
#' @param output either a list ("list"), a data.frame ("df") or a vector of numeric ("num"). Default to "num" if only one ID.
#'
#' @method get_eta mapbayests
#' @return a tibble
#' @export
get_eta.mapbayests <- function(x, ..., output = NULL){

  final_eta <- x$final_eta
  names_final_eta <- names(final_eta[[1]])
  names_dots <- paste0("ETA", unique(unlist(list(...))))

  ok_names_dots <- names_dots[names_dots %in% names_final_eta]

  if(length(ok_names_dots)==0){
    selected_eta <- final_eta
  } else {
    selected_eta <- map(final_eta, ~.x[ok_names_dots])
  }

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

  e <- switch (.out,
    "num" = selected_eta[[1]],
    "list"= selected_eta,
    "df" = bind_rows(selected_eta, .id = "ID")
  )

  return(e)
}
