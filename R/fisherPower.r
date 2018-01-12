#' Power analysis for fisher test
#'
#' Computes the power of fishers exact test by means of simulation.
#'
#' @param pA True proportion in 1st sample. Not to be confused with odds.
#' @param pB True proportion in 2nd sample. Not to be confused with odds.
#' @param nA Size of 1st sample.
#' @param nB Size of 2nd sample.
#' @param alpha Alpha error (default 0.05).
#' @param nRepl Number of simulations to be performed.
#'   
#' @return The power of the test, i.e. 1-Beta.
#'
#' @seealso Method \code{power.fisher.test} from package \code{statmod}
#'   yields essentially the same results.
#'   
#' @author David Kneis \email{david.kneis@@tu-dresden.de}
#'
#' @export
#'
#' @examples
#' nA <- 814
#' pA <- 29/814
#' nB <- 276
#' pB <- 1/276
#' print(fisherPower(pA, pB, nA, nB))
#' \dontrun{
#' library("statmod")
#' power.fisher.test(p1=pA, p2=pB, n1=nA, n2=nB, alpha=0.05, nsim=1000, alternative="two.sided")
#' }

fisherPower <- function(pA, pB, nA, nB, alpha=0.05, nRepl=1e3) {
  nRepl <- 1e3
  is.significant <- function(dummy) {
    a <- ifelse(stats::runif(nA) <= pA, 1, 0)
    b <- ifelse(stats::runif(nB) <= pB, 1, 0)
    m <- matrix(c(sum(a==1), sum(a==0), sum(b==1), sum(b==0)), ncol=2, nrow=2)
    stats::fisher.test(m)$p.value < alpha
  }
  sum(sapply(1:nRepl, is.significant)) / nRepl
}

