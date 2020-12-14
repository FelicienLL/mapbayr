#' Print a mbrests object
#'
#' @param x A \code{mbrests} object.
#' @param ... further arguments passed to or from other methods
#'
#' @method print mbrests
#' @export
print.mbrests <- function(x, ...){
  print(x$mapbay_tab, ...)
}
