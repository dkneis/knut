#' Permutation test for difference in medians of two samples
#'
#' Tests whether the medians of two samples are equal.
#'
#' @param x1 First sample (numeric vector).
#' @param x2 Second sample (numeric vector).
#' @param n Number of replicates (integer).
#' @param name1 Name of 1st sample to be used in output (character string).
#' @param name2 Name of 2nd sample to be used in output (character string).
#' 
#' @return Numeric vector of length 3. The first two elements hold the empirical
#'   sample medians. The third element, named 'p', holds the p-value
#'   representing the probability that the two medians are equal.
#'
#' @note See example below.
#'
#' @seealso No linkes.
#' 
#' @references Based on the code at \url{https://stat.ethz.ch/pipermail/r-help/2010-May/238562.html}.
#'
#' @author David Kneis \email{david.kneis@@tu-dresden.de}
#'
#' @export
#'
#' @examples
#' compareMedians(x1=runif(n=100), x2=rnorm(n=100, mean=0.5, sd=1))

compareMedians <- function(x1, x2, n=2000, name1="median1", name2="median2") {
  x1 <- x1[is.finite(x1)]
  x2 <- x2[is.finite(x2)]
  value <- c(x1, x2)
  group <- c(rep(1, length(x1)), rep(2, length(x2)))
  out <- replicate(n, { tmp <- sample(group); 
    stats::median(value[tmp==1]) - stats::median(value[tmp==2]) } )
  actual <- stats::median(value[group==1]) - stats::median(value[group==2])
  p <- mean(out >= actual)
  stats::setNames(c(stats::median(x1), stats::median(x2), p), c(name1, name2, "p"))
}

