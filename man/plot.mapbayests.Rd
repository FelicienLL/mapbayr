% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mapbayests.R
\name{plot.mapbayests}
\alias{plot.mapbayests}
\title{Plot predictions from mapbayests object}
\usage{
\method{plot}{mapbayests}(x, ..., PREDICTION = c("IPRED", "PRED"))
}
\arguments{
\item{x}{A \code{mapbayests} object.}

\item{...}{additional arguments (passed to \code{\link{augment.mapbayests}})}

\item{PREDICTION}{plot either "IPRED", "PRED" or both.}
}
\value{
a \code{ggplot} object.
}
\description{
Plot predictions from mapbayests object
}
\details{
Use this function to plot the results of the estimations, in the form of concentration vs time profiles for every patient of the data set.
For additional modifications, you can:
\itemize{
\item see \code{\link{augment.mapbayests}} to modify the simulation output.
\item add extra \verb{+function(...)} in order to modify the plot as a regular \code{ggplot2} object.
}
}
\examples{
est <- mapbayest(exmodel(ID = 1))
plot(est, end = 48) +
  ggplot2::labs(title = "Awesome prediction")


}
