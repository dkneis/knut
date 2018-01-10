#' Simulate and compare with observations
#'
#' Runs a model for specific parameter and initial values and arranges
#' times series of simulated and observed values in a data frame
#' (e.g. for plotting or calculation of residuals). 
#'
#' @inheritParams rodeoSimulate
#'
#' @param obs Numeric matrix or data frame with numeric times in the first
#'   column and observed data in the second column.
#' @param extract A function that extracts simulated data from the output matrix
#'   of \code{simul} so that the result can compared with the second column of
#'   \code{obs}.
#'
#' @return Numeric matrix with three colums named 'time', 'sim', and 'obs'.
#'
#' @note The function \code{extract} typically extracts a single column or merges
#'   the values from multiple columns using \code{apply}.
#'
#' @author David Kneis \email{david.kneis@@tu-dresden.de}
#'
#' @export

rodeoCompare <- function(inputs, model, obs, extract) {
  sim <- rodeoSimulate(inputs=inputs, model=model, times=obs[,1])
  cbind(time=obs[,1], obs=obs[,2], sim=extract(sim))
}
