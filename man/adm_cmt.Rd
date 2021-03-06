% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/read_mrgsolve_model.R
\name{adm_cmt}
\alias{adm_cmt}
\title{Get administration compartment numbers from mrgsolve model}
\usage{
adm_cmt(x)
}
\arguments{
\item{x}{model object}
}
\value{
vector of integer
}
\description{
Get administration compartment numbers from mrgsolve model
}
\examples{
model <- mrgsolve::mread("ex_mbr1", mbrlib())
adm_cmt(model)
}
