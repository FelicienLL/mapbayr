#' Print a mbrests object
#'
#' @param x A \code{mbrests} object.
#' @param ... additional arguments
#'
#' @method print mbrests
#' @export
print.mbrests <- function(x, ...){
  NAME <- x$model@model
  nID <- length(x$arg.ofv)
  nOBS <- x$arg.ofv %>% map("DVobs") %>% unname() %>% simplify() %>% length()
  nETA <- n_eta(x)
  ETA <- x$final_eta %>%
    bind_rows(.id = "ID") %>%
    as.data.frame() %>%
    utils::head()
  TAB <- utils::head(as.data.frame(x$mapbay_tab))

  cat("Model: ", NAME, "\n")
  cat("ID :", nID, " individual(s).\n")
  cat("OBS:", nOBS, " observation(s).\n")
  cat("ETA:", nETA, " parameter(s) to estimate.\n\n")
  cat("Estimates: \n")
  print(ETA)
  cat("\nOutput (", nrow(x$mapbay_tab) , " lines): \n", sep = "")
  print(TAB)
}


#' Return the mapbay_tab as a data.frame
#'
#' @param x A \code{mbrests} object.
#' @param row.names,optional,... passed to as.data.frame
#'
#' @method as.data.frame mbrests
#' @export
as.data.frame.mbrests <- function(x, row.names = NULL, optional = FALSE, ...){
  as.data.frame(x$mapbay_tab, ...)
}
