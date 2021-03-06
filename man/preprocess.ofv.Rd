% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mapbayest_check_and_preprocess.R
\name{preprocess.ofv}
\alias{preprocess.ofv}
\alias{preprocess.ofv.fix}
\alias{preprocess.ofv.id}
\title{Preprocess model and data for ofv computation}
\usage{
preprocess.ofv.fix(x, data)

preprocess.ofv.id(x, iddata)
}
\arguments{
\item{x}{the model object}

\item{data, iddata}{NMTRAN-like data set. iddata is likely a dataset of one individual}
}
\value{
a list of arguments use to \code{compute_ofv()}.
}
\description{
Functions to generate arguments passed to \code{\link{compute_ofv}}. Arguments that are fixed between individuals are created once (\code{preprocess.ofv.fix}), while other are specific of each individual (\code{preprocess.ofv.id}).
}
