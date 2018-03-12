#' Generates random sequences of forced extinctions
#'
#' Generates random sequences of forced extinctions. The likelihood of
#'   extinction for a particular species is controlled by a numeric vector of
#'   probabilities. 
#'
#' @param p Named numeric vector of extinction probabilities. Names
#'   represent species. The values should be in range 0, 1 but the do not have
#'   to sum up to 1 (see notes on the \code{prob} argument of function 
#'   \code{\link[base]{sample}}).
#' @param r The number of sequences to be generated. Should be 1 at least.
#' 
#' @return A matrix with \code{r} columns and \code{length(p)} rows filled with
#'   species names sampled from the \code{names(p)}. Each column represents an
#'   individual orderer extinction sequence. Species with larger values of
#'   \code{p} are likely to occur at an earlier position in the sequence, i.e.
#'   closer to the top of the matrix.
#'
#' @author David Kneis \email{david.kneis@@tu-dresden.de}
#'
#' @export
#'
#' @examples
#' # Basic example
#' p <- c("top"= 0.9, "fish1"= 0.8, "fish2"= 0.7, "zoopl"= 0.5, "algae"= 0.1)
#' print(extinctSequences(p, r=5))

extinctSequences <- function(p, r) {
  # Check inputs
  if (!is.vector(p) || !is.numeric(p) || is.null(names(p)) || any(names(p) == "") ||
    any(p < 0) || any(p > 1))
    stop("'p' must be a named numeric vector of probabilities in range 0, 1")
  if (all(p == 0))
    stop("'p' must contain some non-zero values")
  if (!is.numeric(r) || (r < 1) || (r != floor(r)))
    stop("'r' must be an integer >= 1")
  singleSeq <- function(dummy) { sample(names(p), size=length(p), replace=FALSE, prob=p) }
  sapply(1:r, singleSeq)
}

