#' Permutation test for difference in medians of unpaired samples
#'
#' Tests whether the medians of two samples are equal.
#'
#' @param x1 First sample (numeric vector).
#' @param x2 Second sample (numeric vector).
#' @param alternative One of 'two.sided', 'less', or 'greater'.
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
#' x <- c( 18,1.2,2.44,2.35,4.68,7.5 )
#' y <- c( 40.2,11.4,5.78,11.5,9.13,15.3,12.5 )
#' print(compareMedians(x, y, alternative="less"))
#' print(compareMedians(x, y, alternative="two.sided"))
#' print(compareMedians(x, y, alternative="greater"))
#' \dontrun{
#' library("rcompanion")
#' print(percentileTest(x=x, y=y, test="median", r=2000))
#' }

compareMedians <- function(x1, x2, alternative="two.sided", n=2000,
  name1="median1", name2="median2") {
  x1 <- x1[is.finite(x1)]
  x2 <- x2[is.finite(x2)]
  value <- c(x1, x2)
  group <- c(rep(1, length(x1)), rep(2, length(x2)))
  out <- replicate(n, { tmp <- sample(group); 
    stats::median(value[tmp==1]) - stats::median(value[tmp==2]) } )
  actual <- stats::median(value[group==1]) - stats::median(value[group==2])
  if (alternative == "two.sided")
    p <- mean(abs(out) >= abs(actual))
  else if (alternative == "greater")
    p <- mean(out >= actual)
  else if (alternative == "less")
    p <- mean(out <= actual)
  else
    stop("invalid alternative specified")
  stats::setNames(c(stats::median(x1), stats::median(x2), p), c(name1, name2, "p"))
}


