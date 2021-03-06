% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rodeoCompare.r
\name{rodeoCompare}
\alias{rodeoCompare}
\title{Simulate and compare with observations}
\usage{
rodeoCompare(inputs, model, obs, extract, t0 = NULL)
}
\arguments{
\item{inputs}{Named numeric vector. This holds the values of parameters and/or
initial values.}

\item{model}{Object of class \code{rodeo}.}

\item{obs}{Numeric matrix or data frame with numeric times in the first
column and observed data in the remaining column(s).}

\item{extract}{A function that extracts simulated data from the output matrix
of \code{simul}. It should return a matrix of dimensions \eqn{nt * nv}
where \eqn{nt} is the number of model output times and \eqn{nv} equals the
number of value columns in \code{obs}, i.e. excluding the time column.
Typically, the function selects a specific column (or multiple colums) from
the original output matrix of the ODE solver or it computes new column(s)
from the original ones (e.g. the sum of two state variables).}

\item{t0}{Time corresponding to the supplied initial values in argument
\code{inputs}. If \code{NULL}, the element \code{obs[1,1]} will be used.
The value typically needs to be set if the observations are delayed with
respect to the start of the simulation period.}
}
\value{
A data frame with four columns:
\itemize{
  \item{time}: Time
  \item{item}: Name of the observed / simulated entity
  \item{obs}: Observed value
  \item{sim}: Simulated value
}
}
\description{
Runs a z0-dimensional rodeo model for specific parameter and initial values
and returns a list that makes it easy to compare simulated and observed data
(e.g. for plotting or calculating the goodness-of-fit).
}
\note{
The number of rows of the output may be reduced in comparison to the
  number of rows in \code{obs} if \code{t0} is greater than \code{obs[1,1]}.
}
\author{
David Kneis \email{david.kneis@tu-dresden.de}
}
