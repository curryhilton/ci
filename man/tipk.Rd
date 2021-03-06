% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tipk.R
\name{tipk}
\alias{tipk}
\title{Tolerance interval for the population proportion when the population proportion is KNOWN}
\usage{
tipk(x, p, c)
}
\arguments{
\item{x}{A vector of values in a distribution}

\item{p}{A value for the population parameter}

\item{c}{A value for the tolerance level in whole number form...no decimals}
}
\value{
A tolerance interval for the population mean
}
\description{
Tolerance interval for the population proportion when the population proportion is KNOWN
}
\examples{
tipk(attenu$accel, 0.15, 95)
tipk(attenu$accel, 0.15, 99)
}

