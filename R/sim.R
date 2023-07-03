#' Simulate with mapbayr
#'
#' @description A wrapper around [mrgsolve::mrgsim()] for results generated from [mapbayest()]. Exported for the purpose of utility but might be prone to changes.
#'
#' @param x the model object
#' @param data NMTRAN-like data set
#' @param recsort record sorting flag. Defaulted to 3. See [mrgsolve::mrgsim()].
#' @param output type of object returned. Defaulted to `"df"` for a data.frame. See [mrgsolve::mrgsim()].
#' @param ... passed to [mrgsolve::mrgsim()].
#' @param eta a matrix of individual point estimates of ETA. Most likely obtained with [get_eta()].
#' @param nrep number of replicates. If used, the original "ID" in the data will be replaced by unique identifiers.
#' @param new_omega,new_sigma New "omega" and "sigma" matrices to use instead of those defined in "$OMEGA" and "$SIGMA".
#'
#' @return An output from [mrgsolve::mrgsim()].
#' @export
#'
#' @examples
#' library(mrgsolve)
#' mod <- exmodel(1, exdata = FALSE)
#' dat <- exdata(ID = c(1,2))
#'
#' # Classic framework
#' set.seed(123)
#' do_mapbayr_sim(x = mod, data = dat, Request = "DV")
#'
#' # No random effect
#' do_mapbayr_sim(x = zero_re(mod), data = dat)
#' do_mapbayr_sim(x = mod, data = dat, new_omega = "zero_re")
#'
#' # New random effects
#' ## New omega matrix
#' do_mapbayr_sim(x = mod, data = dat, new_omega = dmat(0.1, 0.03, 0.01), nrep = 10)
#'
#' ## Matrix with "eta" as mean and "new_omega" as variance covariance matrix
#' etamat <- get_eta(est001, output = "num")[1:2,]
#'
#' do_mapbayr_sim(
#'   x = mod, data = dat, nrep = 10,
#'   eta = etamat, new_omega = dmat(0.1, 0.03, 0.01)
#' )
#'
do_mapbayr_sim <- function(
    x,
    data,
    recsort = 3,
    output = "df",
    ...,
    eta = NULL,
    nrep = NULL,
    new_omega = NULL, # NULL = keep from the model "x". Accepts a matrix. And also "zero_re", used only if nrep is non-NULL
    new_sigma = NULL  # NULL = keep from the model "x". Accepts a matrix. And also "zero_re", used only if nrep is non-NULL
){

  k <- 1
  if(has_eta_param(x)) k <- 0.5

  nID <- length(unique(data$ID))

  if(!is.null(new_omega)){
    x <- mrgsolve::collapse_omega(x)
    if(is.matrix(new_omega)){
      x <- mrgsolve::omat(x, new_omega)
    }
    if(all(new_omega == "zero_re")){
      x <- zero_re(x, "omega")
    }
  }

  if(!is.null(new_sigma)){
    x <- mrgsolve::collapse_sigma(x)
    if(is.matrix(new_sigma)){
      x <- mrgsolve::smat(x, new_sigma)
    }
    if(all(new_sigma == "zero_re")){
      x <- zero_re(x, "sigma")
    }
  }

  data_to_sim <- data

  if(!is.null(nrep)){
    data_to_sim <- replicate_data(data_to_sim, nrep)
  }

  etasrc <- "omega"
  if(!is.null(eta)){
    if(!is.matrix(eta)){
      eta <- matrix(
        data = eta, nrow = 1,
        dimnames = list(unique(data$ID), names(eta))
      )
    }
    stopifnot(nrow(eta) == nID)

    eta_matrix <- eta

    if(!is.null(nrep)){
      eta_matrix <- matrix(
        data = rep(eta, nrep),
        ncol = ncol(eta),
        byrow = TRUE,
      )
      # Replicated individual point estimates
      # nrow = n(original ID) x n(replicates)

      if(any(omat(x, make = TRUE) != 0)){ # Add "noise" around point estimates
        eta_sim_matrix <- mvgauss(
          mat = omat(x, make = TRUE),
          n = nrep * nID
        ) # nrow = n(original ID) x n(replicates)

        eta_matrix <- eta_matrix + eta_sim_matrix
      }
    }

    eta_matrix <- rename_as_eta(eta_matrix) * k # ETA(1)/2 + ETA1/2
    if(is.matrix(data_to_sim)){
      data_to_sim <- merge_datamatrix_etamatrix(data_to_sim, eta_matrix)
    } else {
      data_to_sim <- merge_datadf_etamatrix(data_to_sim, eta_matrix)
    }

    etasrc <- "data.all"
  }

  ans <- mrgsim(
    x = x,
    data = data_to_sim,
    recsort = recsort,
    output = output,
    ... = ...,
    etasrc = etasrc
  )

  ans
}
