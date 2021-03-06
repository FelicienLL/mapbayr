% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mrgmod.R
\name{data_helpers}
\alias{data_helpers}
\alias{adm_lines}
\alias{obs_lines}
\alias{add_covariates}
\alias{see_data}
\title{Data helpers}
\usage{
adm_lines(x, ...)

obs_lines(x, time, DV, mdv = 0, cmt = NULL, DVmet = NULL, ...)

add_covariates(x, covariates, ...)

see_data(x, ...)
}
\arguments{
\item{x}{model object}

\item{...}{passed to \code{mrgsolve::ev()} in \code{adm_lines()}}

\item{time, DV, mdv, cmt, DVmet}{passed to \code{obs_lines()}}

\item{covariates}{a list of named covariates, with a single value or same number of lines than data}
}
\value{
a \code{mrgmod} object, with a dataset in the \verb{@args$data} slot.
}
\description{
Helpful functions to pass information about administrations (\code{adm_lines()}), observations (\code{obs_lines()}) and covariates (\code{add_covariates()}).
These functions are passed to a \code{mrgmod} object (mrgsolve model), and return a \code{mrgmod} object with a dataset inside, so that mrgsolve or mapbayr functions can be passed along within a pipe-friendly workflow.
}
