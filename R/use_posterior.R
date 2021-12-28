
#' Use posterior estimation
#'
#' @param x A \code{mapbayests} object.
#' @param update_omega Update the OMEGA matrix with the variance-covariance matrix of estimation (a logical, default is `FALSE`).
#' @param update_cov Update the values of covariates with the individual values (a logical, default is `TRUE`).
#' @param update_eta Update the values of ETA with the final estimates (a logical, default is `TRUE`).
#' @param .zero_re Set all elements of the OMEGA or SIGMA matrix to zero. Default is "both" if `update_omega` is FALSE, "sigma" otherwise. (possible values are "both", "sigma", "omega", "none")
#' @param simplify a logical. If TRUE (the default) and only one ID, one mrgmod is returned instead of a list of length 1
#'
#' @details This function takes the results of an estimation (i.e. a \code{mapbayests} object) and return a modified \code{mrgmod} in order to perform \emph{a posteriori} simulations. Modifications are:
#' - If `update_eta` is `TRUE`, the values of ETA are updated to the estimated values (instead of 0) in $PARAM.
#' - If `update_cov` is `TRUE`, the covariates values are updated to the values of the individual (instead of default model values) in $PARAM.
#' - If `update_omega` is `TRUE`, the values of OMEGA are updated with the variance-covariance matrix of estimation (i.e. an approximation of the \emph{a posteriori} distribution) instead of the inter-individual variability (i.e. the \emph{a priori} distribution). Use this command in order to derive a confidence interval of concentrations that reflects the uncertainty about parameter estimation when a large number of profiles are simulated. Note that if inter-individual variability was initially defined in multiple $OMEGA blocks in the model, they will be collapsed to a single full matrix (this is irreversible).
#' - Depending on the values of `.zero_re`, the elements of $OMEGA or $SIGMA can be set to zero, whether you want to simulate one profile, or several in order to derive confidence/prediction intervals.
#' It does not handle time-varying covariates: only the first value will be used as the individual value.
#' @return a mrgmod, or a list of mrgmod if there is more than 1 ID
#' @export
use_posterior <- function(x, update_omega = FALSE, update_cov = TRUE, update_eta = TRUE, .zero_re = NULL, simplify = TRUE){
  if(!inherits(x, "mapbayests")) stop("x is not a mapbayests class object")

  mod <- x$model

  if(isTRUE(update_omega)){
    mod <- mod %>%
      mrgsolve:::collapse_omega() #TO DO ; when the new version of mrgsolve will be on CRAN, use the exported collapse_omega function.
    # For now it "notes" in R CMD check because cannot use unexported functions
    # Also: use the new 'name' argument to name it 'covariance matrix of estimation' or something
  }

  L_mod <- map(seq_along(x$arg.ofv.id), ~mod) %>% set_names(names(x$arg.ofv.id))

  if(isTRUE(update_omega)){
    covariance_matrices <- get_cov(x, simplify = FALSE)
    if(!any(is.na(covariance_matrices))){
      L_mod <- map2(L_mod, covariance_matrices, omat)
    }
  }

  if(isTRUE(update_cov)){
    covs_name <- mbr_cov_names(mod)
    covs_name <- covs_name[!covs_name%in%c("AOLA", "TOLA")]

    cov_values <- get_data(x, output = "list") %>% map(~.x[1,covs_name, drop = FALSE])
    L_mod <- map2(L_mod, cov_values, ~ param(.x, as.list(.y)))

  }

  if(isTRUE(update_eta)){
    L_mod <- map2(L_mod, get_eta(x, output = "list"), ~ param(.x, as.list(.y)))
  }

  if(is.null(.zero_re)){
    if(isTRUE(update_omega)){
      .zero_re <- "sigma"
    } else {
      .zero_re <- "both"
    }
  }

  L_mod <- switch (.zero_re[1],
                 "both" = map(L_mod, zero_re),
                 "omega" = map(L_mod, zero_re, "omega"),
                 "sigma" = map(L_mod, zero_re, "sigma"),
                 "none" = L_mod
  )

  if(length(L_mod) == 1 & isTRUE(simplify)) return(L_mod[[1]])
  L_mod
}
