#' Computes residuals between simulated and observed values
#'
#' Computes residuals. This function is to be passed as argument \code{f} to
#' \code{modFit} from the \code{FME} package. 
#'
#' @inheritParams rodeoSimulate
#' @inheritParams rodeoCompare
#'
#' @param inMod Named numeric vector holding parameters and/or initial values.
#'   These are global in the sense that they are common to all elements of
#'   \code{obs}.
#' @param lst_inFix A list of named numeric vectors. Each vector holds the values
#'   of parameters and/or initial values being constant for the corresponding
#'   set of observations. The length of the list must match the length of the
#'   list \code{obs}.
#' @param lst_obs A list of 2-column matrices (see argument \code{obs} of \code{compare}).
#'   Each list element specifies an independent set of observations.
#' @param transf A function being applied to the observed and simulated data
#'   before computing residuals. Its single argument should be a numeric vector.
#'   The default function doesn't perform any transformation. A typical example
#'   is \code{log10}.
#'
#' @return Numeric vector of residuals. The length of the return vector equals
#'   the total number of observations, i.e. \code{sum(sapply(lst_obs, nrow))}.
#'
#' @note See notes for \code{compare}.
#'
#' @author David Kneis \email{david.kneis@@tu-dresden.de}
#'
#' @export

rodeoResiduals <- function(inMod, lst_inFix, model, lst_obs, extract, transf) {
  if (length(lst_inFix) != length(lst_obs))
    stop("mismatch in length of list arguments")
  out <- c()
  for (i in 1:length(lst_obs)) {
    cmp <- rodeoCompare(inputs=c(inMod, lst_inFix[[i]]), model=model, obs=lst_obs[[i]], extract=extract)
    cmp[,"sim"] <- transf(cmp[,"sim"])
    cmp[,"obs"] <- transf(cmp[,"obs"])
    cmp <- cmp[which(is.finite(cmp[,"obs"]) & is.finite(cmp[,"sim"])), ]
    if (nrow(cmp) == 0)
      warning(paste0("only non-finite residuals for element ",i," of 'obs'"))
    out <- c(out, cmp[,"sim"] - cmp[,"obs"])
  }
  if (length(out) == 0)
    stop("return vector has zero length")
  out
}
