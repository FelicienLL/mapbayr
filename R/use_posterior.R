
#' Use posterior estimation
#'
#' @param x A \code{mapbayests} object.
#' @param update_omega Update the OMEGA matrix with the variance-covariance matrix of estimation (a logical, default is `FALSE`).
#' @param update_cov Update the values of covariates with the individual values (a logical, default is `TRUE`).
#' @param update_eta Update the values of ETA with the final estimates (a logical, default is `TRUE`).
#' @param .zero_re Set all elements of the OMEGA or SIGMA matrix to zero. Default is "both" if `update_omega` is FALSE, "sigma" otherwise. (possible values are "both", "sigma", "omega", "none")
#'
#' @details This function takes the results of an estimation (i.e. a \code{mapbayests} object) and return a modified \code{mrgmod} in order to perform \emph{a posteriori} simulations. Modifications are:
#' - If `update_eta` is `TRUE`, the values of ETA are updated to the estimated values (instead of 0) in $PARAM.
#' - If `update_cov` is `TRUE`, the covariates values are updated to the values of the individual (instead of default model values) in $PARAM.
#' - If `update_omega` is `TRUE`, the values of OMEGA are updated with the variance-covariance matrix of estimation (i.e. an approximation of the \emph{a posteriori} distribution) instead of the inter-individual variability (i.e. the \emph{a priori} distribution). Use this command in order to derive a confidence interval of concentrations that reflects the uncertainty about parameter estimation when a large number of profiles are simulated. Note that if inter-individual variability was initially defined in multiple $OMEGA blocks in the model, they will be collapsed to a single full matrix (this is irreversible).
#' - Depending on the values of `.zero_re`, the elements of $OMEGA or $SIGMA can be set to zero, whether you want to simulate one profile, or several in order to derive confidence/prediction intervals.
#' This function works if there is only one individual in the dataset. It does not handle time-varying covariates.
#' @return a mrgmod
#' @export
use_posterior <- function(x, update_omega = FALSE, update_cov = TRUE, update_eta = TRUE, .zero_re = NULL){
  if(!inherits(x, "mapbayests")) stop("x is not a mapbayests class object")

  mod <- x$model

  if(length(x$arg.ofv.id) > 1) stop("use_posterior() can be used with one only ID", call. = FALSE)

  if(isTRUE(update_omega)){
    covariance_matrix <- get_cov(x)
    if(!any(is.na(covariance_matrix))){
      mod <- mod %>%
        mrgsolve:::collapse_omega() %>% #TO DO ; when the new version of mrgsolve will be on CRAN, use the exported collapse_omega function.
        # For now it "notes" in R CMD check because cannot use unexported functions
        # Also: use the new 'name' argument to name it 'covariance matrix of estimation' or something
        omat(covariance_matrix)
    }
  }

  if(isTRUE(update_cov)){
    covs_name <- mbr_cov_names(mod)
    covs_name <- covs_name[!covs_name%in%c("AOLA", "TOLA")]

    is_tv <- (map_dbl(covs_name, ~length(unique(x$mapbay_tab[[.x]]))) != 1) #is time-varying?

    if(any(is_tv)) warning("Time-varying covariates found. First value used for: ",  paste(covs_name[is_tv], collapse = ", "), ".")

    covs <- x$mapbay_tab[1,covs_name, drop = FALSE]

    mod <- mod %>% param(as.list(covs))

  }

  if(isTRUE(update_eta)){
    etas <- x$final_eta[[1]]
    mod <- mod %>% param(as.list(etas))
  }

  if(is.null(.zero_re)){
    if(isTRUE(update_omega)){
      .zero_re <- "sigma"
    } else {
      .zero_re <- "both"
    }
  }

  mod <- switch (.zero_re[1],
                 "both" = zero_re(mod),
                 "omega" = zero_re(mod, "omega"),
                 "sigma" = zero_re(mod, "sigma"),
                 "none" = (mod)
  )

  mod
}
