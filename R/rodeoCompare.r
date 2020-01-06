#' Simulate and compare with observations
#'
#' Runs a z0-dimensional rodeo model for specific parameter and initial values
#' and returns a list that makes it easy to compare simulated and observed data
#' (e.g. for plotting or calculating the goodness-of-fit). 
#'
#' @inheritParams rodeoSimulate
#'
#' @param obs Numeric matrix or data frame with numeric times in the first
#'   column and observed data in the remaining column(s).
#' @param extract A function that extracts simulated data from the output matrix
#'   of \code{simul}. It should return a matrix of dimensions \eqn{nt * nv}
#'   where \eqn{nt} is the number of model output times and \eqn{nv} equals the
#'   number of value columns in \code{obs}, i.e. excluding the time column.
#'   Typically, the function selects a specific column (or multiple colums) from
#'   the original output matrix of the ODE solver or it computes new column(s)
#'   from the original ones (e.g. the sum of two state variables).
#' @param t0 Time corresponding to the supplied initial values in argument
#'   \code{inputs}. If \code{NULL}, the element \code{obs[1,1]} will be used.
#'   The value typically needs to be set if the observations are delayed with
#'   respect to the start of the simulation period.
#'
#' @return A data frame with four columns:
#' \itemize{
#'   \item{time}: Time
#'   \item{item}: Name of the observed / simulated entity
#'   \item{obs}: Observed value
#'   \item{sim}: Simulated value
#' }
#'
#' @note The number of rows of the output may be reduced in comparison to the
#'   number of rows in \code{obs} if \code{t0} is greater than \code{obs[1,1]}.
#'
#' @author David Kneis \email{david.kneis@@tu-dresden.de}
#'
#' @export

rodeoCompare <- function(inputs, model, obs, extract, t0=NULL) {
  if ((!is.matrix(obs)) || (!is.numeric(obs)))
    stop("'obs' must be a numeric matrix")
  if (!is.null(t0) && ((!is.numeric(t0)) || (length(t0) != 1)))
    stop("'t0' must be a numeric scalar unless it is NULL")
  if (!is.function(extract))
    stop("'extract' must be a function")
  if (is.null(t0)) {
    times <- obs[,1]
  } else {
    times <- c(t0, obs[(obs[,1] > t0), 1])
    if (length(times) < 2)
      stop("use of 't0' results in a vector of times with less than 2 elements")
  }
  sim <- rodeoSimulate(inputs=inputs, model=model, times=times)
  sim <- sim[sim[,1] %in% obs[,1],]  # drop result for t0 earlier than obs[1,1]
  sim <- extract(sim)
  if (!is.matrix(sim))
    stop("extracted model output is not a matrix")
  if (sort(colnames(sim)[2:ncol(sim)]) != sort(colnames(obs)[2:ncol(obs)])) {
    stop(paste0("observed data have column(s) '",
      paste(colnames(obs)[2:ncol(obs)], collapse="', '"),
      "' but simulated data after extraction have column(s) '",
      paste(colnames(sim)[2:ncol(sim)], collapse="', '"),"'")) 
  }
  out <- NULL
  for (it in colnames(obs)[2:ncol(obs)]) {
    out <- rbind(out,
      data.frame(item=it, time=obs[,1], obs=obs[,it], sim=sim[,it],
        stringsAsFactors=FALSE))
  }
  out
}

