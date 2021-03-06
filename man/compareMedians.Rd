% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/compareMedians.r
\name{compareMedians}
\alias{compareMedians}
\title{Permutation test for difference in medians of unpaired samples}
\usage{
compareMedians(x1, x2, alternative = "two.sided", n = 2000,
  name1 = "median1", name2 = "median2")
}
\arguments{
\item{x1}{First sample (numeric vector).}

\item{x2}{Second sample (numeric vector).}

\item{alternative}{One of 'two.sided', 'less', or 'greater'.}

\item{n}{Number of replicates (integer).}

\item{name1}{Name of 1st sample to be used in output (character string).}

\item{name2}{Name of 2nd sample to be used in output (character string).}
}
\value{
Numeric vector of length 3. The first two elements hold the empirical
  sample medians. The third element, named 'p', holds the p-value
  representing the probability that the two medians are equal.
}
\description{
Tests whether the medians of two samples are equal.
}
\note{
See example below.
}
\examples{
x <- c( 18,1.2,2.44,2.35,4.68,7.5 )
y <- c( 40.2,11.4,5.78,11.5,9.13,15.3,12.5 )
print(compareMedians(x, y, alternative="less"))
print(compareMedians(x, y, alternative="two.sided"))
print(compareMedians(x, y, alternative="greater"))
\dontrun{
library("rcompanion")
print(percentileTest(x=x, y=y, test="median", r=2000))
}
}
\references{
Based on the code at \url{https://stat.ethz.ch/pipermail/r-help/2010-May/238562.html}.
}
\seealso{
No linkes.
}
\author{
David Kneis \email{david.kneis@tu-dresden.de}
}
