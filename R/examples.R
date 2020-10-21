#' Internal "mapbayr" model examples
#'
#' @export
#'
#' @examples
#' \dontrun{
#' mod <- mrgsolve::mread("ex_mbr1", mbrlib())
#' mod <- mrgsolve::mread("ex_mbr2", mbrlib())
#' mod <- mrgsolve::mread("ex_mbr3", mbrlib())
#' }
mbrlib <- function(){
  file.path(path.package("mapbayr"), "models")
}
