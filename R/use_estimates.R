#' Use parameter estimates
#'
#' @param x A \code{mapbayests} object.
#' @param use_eta a logical. Populate the `idataset` with the estimates (ETA).
#' @param use_covariates a logical. Populate the `idataset` with the covariate values available in the data.
#' @param .etasrc a character. Value used to populate `mrgsim(etasrc)`.
#' @param .zero_re a character. Set all elements of the OMEGA or SIGMA matrix to zero. Default is "both", alternatively "sigma", "omega" and "none".
#' @param verbose a logical. Display information to the console.
#'
#' @returns An mrgsolve model object (of class "mrgmod").
#' @details This function takes the results of an estimation (i.e. a \code{mapbayests} object) and return a modified \code{mrgmod} in order to perform \emph{a posteriori} simulations. Modifications are:
#' - An individual data set (`idata_set`), populated with the estimated ETA parameters and the covariate values provided in the original data set.
#' - The argument `etasrc` set to "idata.all" (or different, depending on the value set to `.etasrc`).
#' - OMEGA and SIGMA matrices set to zero (or different, depending on the value of `.zero_re`).
#' It does not handle time-varying covariates: only the first value will be used as the individual value.
#' @export
#'
#' @examples
#' library(magrittr)
#' library(mrgsolve)
#' est <- mapbayest(exmodel(ID = 1:8))
#' est %>%
#'   use_estimates() %>%
#'   ev(amt = 50000) %>%
#'   mrgsim() %>%
#'   plot()
use_estimates <- function(
    x,
    use_eta = TRUE,
    use_covariates = TRUE,
    .etasrc = "idata.all",
    .zero_re = "both",
    verbose = TRUE
){
  if(!inherits(x, "mapbayests")) stop("x is not a mapbayests class object")

  mod <- x$model
  idataset <- data.frame(ID = unique(get_data(x)$ID))
  information <- character(0)

  if(use_eta){
    idataset <- left_join(
      x = idataset,
      y = get_eta.mapbayests(x, output = "df"),
      by = "ID"
    )
    if(verbose){
      information <- c(information, i = "Updating `idata_set()` with individual ETA estimates.")
    }
  }

  allcovariatenames <- setdiff(mbr_cov_names(mod), c("AOLA", "TOLA"))
  providedcovariates <- intersect(allcovariatenames, names(get_data(x)))

  if(length(providedcovariates) && use_covariates){
    idataset <- left_join(
      x = idataset,
      y = x %>%
        get_data() %>%
        dplyr::select(all_of(c("ID", providedcovariates))) %>%
        distinct(.data[["ID"]], .keep_all = TRUE),
      by = "ID"
    )
    if(verbose){
      information <- c(information, i = "Updating `idata_set()` with the covariate values provided in the data.")
    }
  }

  if(ncol(idataset) > 1){
    mod <- mod %>%
      idata_set(idataset)
  }

  if(.zero_re == "both"){
    mod <- zero_re(mod)
    if(verbose){
      information <- c(information, i = "Setting all elements of the OMEGA and SIGMA matrices to zero.")
    }
  }
  if(.zero_re == "omega"){
    mod <- zero_re(mod, "omega")
    if(verbose){
      information <- c(information, i = "Setting all elements of the OMEGA matrix to zero.")
    }
  }
  if(.zero_re == "sigma"){
    mod <- zero_re(mod, "sigma")
    if(verbose){
      information <- c(information, i = "Setting all elements of the SIGMA matrix to zero.")
    }
  }
  if(.zero_re == "none"){
    if(verbose){
      information <- c(information, i = "Leaving the OMEGA or SIGMA matrices unmodified.")
    }
  }

  mod@args$etasrc <- .etasrc
  if(verbose){
    information <- c(information, i = paste0("Setting `etasrc = \"", .etasrc, "\"`."))
    information <- c(information, i = "You can use `data_set()` or `ev()` to simulate \"a posteriori\".")
  }

  rlang::inform(information)

  mod
}
