#' Title
#'
#' @return Called for its side effects.
#' @export
#'
#' @importFrom shiny runApp
#'
#'
launch_app_tki <- function(){
  if (!requireNamespace("shiny", quietly = TRUE)) {
    stop("Please install `shiny` before running the app.",
         call. = F)
  }
  shiny::runApp(system.file('tki', package='mapbayr'), launch.browser = T)
  invisible(NULL)
}

#' Title
#'
#' @return Called for its side effects.
#' @export
#'
#' @importFrom shiny runApp
#'
#'
launch_app_carbo <- function(){
  if (!requireNamespace("shiny", quietly = TRUE)) {
    stop("Please install `shiny` before running the app.",
         call. = F)
  }
  shiny::runApp(system.file('carbo', package='mapbayr'), launch.browser = T)
  invisible(NULL)
}
