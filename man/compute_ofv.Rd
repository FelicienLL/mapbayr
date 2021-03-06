% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mapbayest_ofv_computation.R
\name{compute_ofv}
\alias{compute_ofv}
\title{Compute the objective function value}
\usage{
compute_ofv(
  eta,
  mrgsolve_model,
  data,
  sigma,
  log_transformation,
  DVobs,
  omega.inv,
  obs_cmt
)
}
\arguments{
\item{eta}{a named vector of numeric (ETA1, ETA2...)}

\item{mrgsolve_model}{model object (updated for purpose)}

\item{data}{NMTRAN-like data (of one individual)}

\item{sigma}{matrix}

\item{log_transformation}{logical. If \code{TRUE}, prediction will be log-transformed to compute ofv (useful for log-additive model)}

\item{DVobs}{vector of observation to fit}

\item{omega.inv}{inverse of omega matrix}

\item{obs_cmt}{vector of compartments with observations to fit}
}
\value{
a single numeric value (the objective function value)
}
\description{
Compute the objective function value
}
\details{
This function is called iteratively by the optimization function. Arguments should not be passed directly, but generated by the pre-processing functions (see \code{\link{preprocess.ofv}})
}
