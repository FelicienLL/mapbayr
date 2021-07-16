#' Get content from object
#'
#' @name get_x
#' @param x object to get content from
#' @param ... passed along
#' @return the class of the object returned depends on the function, and on their arguments. Typically, a data.frame or a vector if the output can be reduced to one line.
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

#' @rdname get_x
#' @export
get_param <- function(x, ...) UseMethod("get_param")

#' Return a posteriori param from a mapbayests
#'
#' @param x mapbayests object
#' @param ... passed along
#' @param output either a data.frame ("df") or a vector of numeric ("num"). Default to "num" if only one ID
#' @param keep_ID a logical. By default, the ID variable is dropped if one ID in data.
#' @param keep_names a logical. By default, names are dropped if one parameter is requested, and output is not a data frame.
#'
#' @method get_param mapbayests
#' @return a tibble
#' @export
get_param.mapbayests <- function(x, ..., output = NULL, keep_ID = NULL, keep_names = NULL){
  #Check Arguments
  stopifnot((is.logical(keep_ID)|is.null(keep_ID)), (is.logical(keep_names)|is.null(keep_names)))

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
  onepar <- (length(ok_names) == 1)

  if(is.null(output)){
    if(oneID){
      .out <- "num"
    } else {
      .out <- "df"
    }
  } else {
    okout <- c("num", "df")
    if(!output %in% okout) stop('Allowed output are: ', paste(okout, collapse = ", "), ".")
    .out <- output[1]
  }

  if(!oneID & .out == "num") stop("Multiple ID, cannot coerce list to a vector of numeric.")

  if(is.null(keep_ID)){
    if(oneID){
      .keep_ID <- FALSE
    } else {
      .keep_ID <- TRUE
    }
  } else {
    .keep_ID <- keep_ID
  }

  if(is.null(keep_names)){
    if(onepar & oneID){
      .keep_names <- FALSE
    } else {
      .keep_names <- TRUE
    }
  } else {
    .keep_names <- keep_names
  }

  par_tab <- x$mapbay_tab %>%
    select(.data$ID, dplyr::any_of(ok_names)) %>%
    group_by(.data$ID) %>%
    dplyr::slice(1) %>%
    dplyr::ungroup()

  if(!.keep_ID) par_tab <- select(par_tab, -.data$ID)
  if(!.keep_names) unname(as.data.frame(par_tab))

  par <- switch (.out,
                 "num" = unlist(par_tab),
                 "df"= par_tab
  )

  return(par)
}
