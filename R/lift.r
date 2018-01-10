#' Linear fitting with interactive data selection
#'
#' The function fits linear models to a subset of data that is interactively
#' selected from a scatter plot.
#'
#' @param df A data frame with columns specified in the other arguments.
#' @param time Name of the column of \code{df} holding numeric times
#'   corresponding to observations. Should be a character string.
#' @param value Name of the column of \code{df} holding the observed
#'   time dependent quantity, e.g. optical density. Should be a character string.
#' @param keys Name(s) of the column(s) of \code{df} that define subsets of the
#'   data to be analyzed separatety. Should be a vector of character strings
#'   which can be of length one. The specified columns should be of type
#'   character, integer, or factor. They typically hold experiment IDs and/or
#'   replicate IDs.
#' @param trans A function that is applied to the data in the \code{value}
#'   column of \code{df}. The default, \code{\link[base]{log10}}, is useful
#'   for microbial growth data, for example.
#'   
#' @return A data frame with one row for each unique combination of the key
#'   columns of \code{df}. The number of columns is \code{length(keys)} plus 2.
#'   The two additional columns are named 'slope.default' and 'slope.robust'
#'   where the former holds the slope of a linear model fitted with
#'   \code{\link[stats]{lm}} and the latter holds the slope obtained with
#'   \code{\link[robust]{lmRob}}.
#'
#' @note A plot is created for each subset of the data and the user is asked to
#'   select a (time) range on the x-axis by two sebsequent clicks with the left
#'   mouse button. The selected data points are highlighted and the fitted
#'   models are displayed. The selection can be corrected by repeating the
#'   two subsequent left-clicks. A click with the right mouse button (or
#'   pressing the 'finish' button in Rstudio) accepts
#'   the current result and the process continues with the next subset of the
#'   data (i.e. the next plot). The results is returned after all subsets
#'   were processed.
#'
#' @author David Kneis \email{david.kneis@@tu-dresden.de}
#'
#' @export
#'
#' @examples
#' \dontrun{
#' d <- read.table(header=TRUE, sep="", text='
#'   ID  time  value
#'   1   1     1
#'   1   2     2
#'   1   3     3
#'   2   1     1
#'   2   2     3
#'   2   3     2
#' ')
#' print(lift(d))
#' }

lift <- function(df, time="time", value="value", keys=c("ID"), trans=log10) {
  if (!time %in% names(df))
    stop("time column not present in data")
  if (!value %in% names(df))
    stop("value column not present in data")
  if (!all(keys %in% names(df)))
    stop("key column(s) not present in data")
  df[,value] <- trans(df[,value])
  out <- unique(df[,keys,drop=FALSE])
  slopes <- matrix(NA, ncol=2, nrow=nrow(out),
    dimnames=list(NULL, c("slope.default","slope.robust")))
  coeffs_empty <- matrix(NA, ncol=2, nrow=2,
    dimnames=list(c("default","robust"),c("intercept","slope")))
  for (i in 1:nrow(out)) {
    main <- paste0("Data set ",i," of ",nrow(out))
    this <- rep(TRUE, nrow(df))
    for (k in keys)
      this[df[,k] != out[i,k]] <- FALSE
    xy <- df[this, c(time, value), drop=FALSE]
    graphics::plot(xy, main=main)
    coeffs <- coeffs_empty  # if no selection is made
    clicks <- 0
    while (TRUE) {
      loc <- graphics::locator(n=1)
      # exit if right button was pressed
      if (is.null(loc))
        break
      # re-initialize
      coeffs <- coeffs_empty  # if previous selection is revised
      # if left button
      clicks <- clicks + 1
      if ((clicks %% 2) != 0) {  # first click
        loc1 <- loc
        graphics::plot(xy, main=main)
        graphics::abline(v=loc1$x)
      } else {                   # second click
        loc2 <- loc
        graphics::plot(xy, main=main)
        graphics::abline(v=c(loc1$x, loc2$x))
        sel <- (xy[,"time"] >= min(loc1$x, loc2$x)) &
               (xy[,"time"] <= max(loc1$x,loc2$x))
        if (length(sel) > 0) {
          graphics::points(xy[sel,], pch=20, col="steelblue2")
          if (length(sel) > 1) {
            coeffs["default",] <- stats::coef(
              stats::lm(xy[sel,value] ~ xy[sel,time]))
            graphics::lines(xy[sel,"time"],
              coeffs["default",1] + coeffs["default",2] * xy[sel,"time"],
              col="red", lty=1)
            coeffs["robust",] <- stats::coef(
              robust::lmRob(xy[sel,value] ~ xy[sel,time]))
            graphics::lines(xy[sel,"time"],
              coeffs["robust",1] + coeffs["robust",2] * xy[sel,"time"],
              col="red", lty=2)
            legend("topleft", bty="n", lty=1:2, col="red", legend=c("default", "robust"))
            legend("bottomright", bty="n",
              legend=c(paste("default:",signif(coeffs["default",2],3)),
                       paste("robust:",signif(coeffs["robust",2],3))))
          }
        }
      }
    }
    slopes[i,"slope.default"] <- coeffs["default",2]
    slopes[i,"slope.robust"] <- coeffs["robust",2]
  }
  grDevices::graphics.off()
  cbind(out, slopes)
}
