#' Run simulation
#'
#' Runs a simulation of a 0-dimensional \code{rodeo}-based model. 
#'
#' @param inputs Named numeric vector. This holds the values of parameters and/or
#'   initial values.
#' @param model Object of class \code{rodeo}.
#' @param times Numeric vector specifying the desired output times.
#'
#' @return Numeric matrix with times in the first column and states or
#'   process rates in the remaining columns.
#'
#' @author David Kneis \email{david.kneis@@tu-dresden.de}
#'
#' @export

rodeoSimulate <- function(inputs, model, times) {
  model$setPars(inputs[model$namesPars()])
  model$setVars(inputs[model$namesVars()])
  tryCatch({
    sim <- model$dynamics(times)
  }, error= function(x) {
    print(x)
    stop("simulation failed for inputs: ",
      paste(names(inputs), inputs, sep="=", collapse=", "))
  })
  sim
}
