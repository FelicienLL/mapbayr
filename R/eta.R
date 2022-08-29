#' Generate a vector of "ETA"
#'
#' @param x either a `mrgsolve` model object, or a numeric
#' @param ... additional numeric(s)
#' @param n,val generate a sequence of `val` of length `n`
#'
#' @return a single named vector of numeric
#' @export
#'
#' @description
#' Generate a vector of "ETA" values. If x is a `mrgsolve` model, these will be extracted from values defined in `$PARAM`. Otherwise, any numeric values passed to `x` and `...` as vector(s) or list(s) will be coerced as a single vector. Alternatively, if `x` and `...` are missing, generate a vector of ETA equal to `val` of length `n`.
#'
#' @examples
#' # Extract ETA from the model
#' mod <- exmodel()
#' eta(mod)
#'
#' # Coerce numeric values
#' eta(0.1, 0.2, c(0.3, 0.4), list(0.5, 0.6))
#' eta(rnorm(4))
#'
#' # Generate a sequence from scratch
#' eta(n = 3)
#' eta(n = 3, val = 0.001)
eta <- function(x, ..., n, val = 0){
  if(missing(x)){
    if(missing(n)){
      return(NULL)
    } else {
      ans <- rep_len(val, length.out = n)
      return(rename_as_eta(ans))
    }
  }
  if(is.mrgmod(x)){
    par <- as.double(param(x))
    ans <- par[grepl("^ETA\\d+$", names(par))]
    if(length(ans) == 0) ans <- NULL
    return(ans)
  }
  dots <- list(...)
  xdots <- c(list(x), ...)
  arenum <- sapply(xdots, is.numeric)
  if(any(!arenum)) stop(paste0(xdots[!arenum], sep = ", "), "is (are) not numeric")
  ans <- unlist(xdots)
  return(rename_as_eta(ans))
}

# x, a vector or a matrix to be renamed
rename_as_eta <- function(x){
  if(is.matrix(x)){
    colnames(x) <- paste0("ETA", seq_len(ncol(x)))
    return(x)
  }
  names(x) <- paste0("ETA", seq_along(x))
  x
}

eta_length <- function(...){
  length(eta(...))
}

eta_names <- function(...){
  names(eta(...))
}
