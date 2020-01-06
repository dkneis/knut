#' Simulate and compare with observations for multiple data sets
#'
#' Extends \code{rodeoCompare} for use with multiple data sets. 
#'
#' @inheritParams rodeoSimulate
#' @inheritParams rodeoCompare
#'
#' @param globals Named numeric vector holding parameters and/or initial values.
#'   These are global in the sense that they are common to all elements of
#'   \code{obs}.
#' @param locals A numeric matrix with row and column names. Each row holds the
#'   values of parameters and/or initial values applying to a specific
#'   set of observations. The matching between \code{locals} and \code{lst_obs}
#'   is deduced from the row and element names, respectively.
#' @param lst_obs A list of numeric matrices, each of which holds numeric times in
#'   the first column and values of observed variables in the remaining columns.
#'
#' @return A data frame with five columns:
#' \itemize{
#'   \item{id}: Name of the set of observations
#'   \item{item}: Name of the observed / simulated entity
#'   \item{time}: Time
#'   \item{obs}: Observed value
#'   \item{sim}: Simulated value
#' }
#'
#' @note No notes.
#'
#' @author David Kneis \email{david.kneis@@tu-dresden.de}
#'
#' @export

rodeoCompareAll <- function(globals, locals, model, lst_obs, extract, t0=NULL) {
  if (sort(rownames(locals)) != sort(names(lst_obs)))
    stop("rownames of 'locals' do not match with names of 'lst_obs'")
  out <- NULL
  for (n in names(lst_obs)) {
    z <- rodeoCompare(inputs=c(globals, locals[n,]), model=model,
      obs=lst_obs[[n]], extract=extract, t0=t0)
    for (it in colnames(z$obs)) {
      out <- rbind(out,
        data.frame(id=n, item=it, time=z$time, obs=z$obs[,it], sim=z$sim[,it],
          stringsAsFactors=FALSE))
    }
  }
  out
}
