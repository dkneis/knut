#' Estimate parameters
#'
#' Estimates parameters using \code{modFit} from the \code{FME} package.
#'
#' @inheritParams rodeoSimulate
#' @inheritParams rodeoCompare
#' @inheritParams rodeoResiduals
#'
#' @param inMod.guess Initial guess of the parameters / initial values to be
#'   optimized. Named numeric vector.
#' @param inMod.lower Lower bounds of the parameters / initial values to be
#'   optimized. Named numeric vector.
#' @param inMod.upper Upper bounds of the parameters / initial values to be
#'   optimized. Named numeric vector.
#'
#' @return A list with the following components:
#' \itemize{
#'   \item{estimates} Named vector of the estimated parameters.
#'   \item{stdErrors} Named vector of the parameters' standard errors.
#'   \item{residuals} Numeric vector of residuals (e.g. for calculation of RMSE).
#'   \item{info} Return code of optimizer. See help for \code{summary.modFit}.
#' }
#'
#' @note See notes for \code{compare}.
#'
#' @author David Kneis \email{david.kneis@@tu-dresden.de}
#'
#' @export

rodeoEstimate <- function(inMod.guess, inMod.lower, inMod.upper, lst_inFix, model, lst_obs, extract, transf) {
  if (any(names(inMod.lower) != names(inMod.guess)) || any(names(inMod.upper) != names(inMod.guess)))
    stop("length/names of 'inMod.*' not consistent")
  out <- list(
    estimates= stats::setNames(rep(NA, length(inMod.guess)), names(inMod.guess)),
    stdErrors= stats::setNames(rep(NA, length(inMod.guess)), names(inMod.guess)),
    residuals= NA,
    info=NA
  )
  tryCatch({
    est <- FME::modFit(f=rodeoResiduals, p=inMod.guess, lst_inFix=lst_inFix,
      model=model, lst_obs=lst_obs, extract=extract, transf=transf,
      lower=inMod.lower, upper=inMod.upper, method="Marq")
    pars <- summary(est)$par[,1:2]
    out <- list(
      estimates= stats::setNames(pars[,1], rownames(pars)),
      stdErrors= stats::setNames(pars[,2], rownames(pars)),
      residuals= est$residuals,
      info=est$info
    )
  }, error= function(x) {
    print("estim: Error thrown for guess:")
    print(inMod.guess)
    stop(x)
  })
  out
}
