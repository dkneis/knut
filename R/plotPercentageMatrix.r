#' Plots a numeric matrix using a color scale
#'
#' Plots a numeric matrix using a color scale.
#' 
#' @param x Numeric matrix with column and row names.
#' @param breaks Numeric vector defining breaks in the legend. This typically
#'   includes the lower limit (e.g. zero for percentage data) but not the
#'   upper limit. The default is useful for fraction data (range 0...1).
#' @param colors A vector of color names to build the palette.
#' @param useHatching Use hatching to improve the distinction between
#'   neighboring colors? (logical)
#' @param translateRownames Function to translate row names. Must accept a vector
#'   and return a vector of character strings of the same length as the input
#' @param translateColnames Function to translate column names. Must accept a vector
#'   and return a vector of character strings of the same length as the input
#' @param mar Vector of length 4 passed to the \code{mar} element of
#'   \code{\link[graphics]{par}}. The last element (right margin) must be chosen
#'   large enough so as to not cut off the automatic legend.
#' @param rowGroups A vector of e.g. factors splitting the rows of \code{x}
#'   into groups. Lines will be plotted at the transition between groups.
#' 
#' @return \code{invisible(NULL)}.
#'
#' @note Note that the matrix is plotted in transposed form.
#'
#' @seealso No links.
#' 
#' @references No references.
#'
#' @author David Kneis \email{david.kneis@@tu-dresden.de}
#'
#' @export
#'
#' @examples
#'x <- matrix(runif(24), nrow=4, ncol=6,
#'  dimnames=list(paste("row", 1:4), paste("col", 1:6)))
#'plotColorMatrix(x=x, breaks=c(50,20,10,5,2,0)/100,
#'  useHatching=TRUE, translateColnames=function(x){sapply(x, gsub,
#'    patter="col", replacement="column", fixed=TRUE)}, rowGroups= c(1,1,2,2))
#'plotColorMatrix(x=x, breaks=c(50,20,10,5,2,0)/100,
#'  useHatching=FALSE, translateColnames=function(x){sapply(x, gsub,
#'  patter="col", replacement="column", fixed=TRUE)}, rowGroups= c(1,1,2,2))

plotColorMatrix <- function(
  x,
  breaks=c(50,20,10,5,2,0)/100,
  colors=if (useHatching) c("black", "white") else c("orangered3","khaki","royalblue4"),
  useHatching=FALSE,
  translateRownames=function(x){x},
  translateColnames=function(x){x},
  mar=c(5,5,1,8),
  rowGroups= rep(1, nrow(x))
) {
  breaks <- sort(breaks, decreasing=TRUE)
  # Define helper functions first
  ramp <- grDevices::colorRampPalette(colors)(length(breaks))
  clr <- function(x, breaks) {
    cols <- grDevices::colorRampPalette(ramp)(length(breaks))
    for (i in 1:length(breaks)) {
      if (x >= breaks[i]) return(cols[i])
    }
    return("black")
  }
  if (useHatching) {
    dens <- function(x, breaks) {
      dens <- rep(c(0, 20), length(breaks))
      for (i in 1:length(breaks)) {
        if (x >= breaks[i]) return(dens[i])
      }
      return(0)
    }
  } else {
    dens <- function(x, breaks) {
      return(0)
    }
  }
  # Actual plotting starts here
  omar <- graphics::par("mar")
  graphics::par(mar=mar)
  graphics::plot(c(0.5,nrow(x)+0.5), c(0.5,ncol(x)+0.5), type="n", xlab="", ylab="",
    xaxt="n", yaxt="n", bty="n")
  graphics::axis(1, at=1:nrow(x), label=translateRownames(rownames(x)), las=2, lwd=0, line=-1)
  graphics::axis(2, at=1:ncol(x), label=translateColnames(colnames(x)), las=1, lwd=0, line=-1)
  delta <- 0.3
  for (nc in 1:ncol(x)) {
    for (nr in 1:nrow(x)) {
      graphics::rect(xleft=nr-delta, xright=nr+delta, ybottom=nc-delta, ytop=nc+delta,
        col=clr(x[nr,nc], breaks))
      graphics::rect(xleft=nr-delta, xright=nr+delta, ybottom=nc-delta, ytop=nc+delta,
        col="white", density=dens(x[nr,nc], breaks), border="darkgrey")
      if ((nr > 1) && (rowGroups[nr] != rowGroups[nr-1]))
        graphics::lines(x=rep(nr-0.5, 2), y=c(1-delta*1.8,ncol(x)+delta*1.5))
    }
  }
  rev <- length(breaks):1
  labs <- paste0(">",breaks[rev]*100,"%")
  labs[1] <- gsub(labs[2], pattern="^>(.+)", replacement="<\\1")
  pos <- c(x=nrow(x)+1.2, y=ncol(x))
  graphics::legend(pos["x"], pos["y"], bty="n", xpd=TRUE, fill=sapply(breaks, clr, breaks)[rev],
    legend=labs, adj=c(0.25,0.3), text.col="white", border="darkgrey")
  graphics::legend(pos["x"], pos["y"], bty="n", xpd=TRUE, density=sapply(breaks, dens, breaks)[rev],
    fill="white", legend=labs, adj=c(0.25,0.3), border="darkgrey")
  graphics::par(mar=omar)
}
