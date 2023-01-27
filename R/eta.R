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
    return(sort_eta(ans))
  }
  dots <- list(...)
  xdots <- c(list(x), ...)
  arenum <- sapply(xdots, is.numeric)
  if(any(!arenum)) stop(paste0(xdots[!arenum], sep = ", "), "is (are) not numeric")
  ans <- unlist(xdots)
  return(rename_as_eta(ans))
}


make_eta_names <- function(x, n){
  if(!missing(n)){
    x <- seq_len(n)
  }
  paste0("ETA", x)
}

# x, a vector or a matrix to be renamed
rename_as_eta <- function(x){
  if(is.matrix(x)){
    colnames(x) <- make_eta_names(n = ncol(x))
    return(x)
  }
  names(x) <- make_eta_names(n = length(x))
  x
}

sort_eta <- function(x){
  x[order(as.numeric(gsub("ETA", "", names(x))))]
}

eta_length <- function(...){
  length(eta(...))
}

eta_names <- function(...){
  names(eta(...))
}

#' Fill a vector/matrix of ETAs
#'
#' @param x a named vector of ETAs, or a matrix to fill
#' @param n the maximum number of ETAs
#'
#' @return a object of the same type as x, with adequate dimensions
#' @noRd
#' @examples
#' fill_eta(c(ETA2 = 2, ETA4 = -4), n = 5)
fill_eta <- function(x, n){
  if(missing(n)) stop("n is missing")
  if(is.matrix(x)){
    y <- rename_as_eta(matrix(0, nrow = nrow(x), ncol = n))
    y[,colnames(x)] <- x
    row.names(y) <- row.names(x)
  } else {
    y <- eta(n = n, value = 0)
    y[names(x)] <- x
  }
  y
}


