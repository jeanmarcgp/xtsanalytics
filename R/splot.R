#  FUNCTION  splot.R
#  -----------------
#
#'  Simple scatterplot of two xts matrices with linear regression.
#'
#'  Performs a scatterplot of two xts matrix columns and does a linear
#'  regression on the results.
#'
#' @param x     Scatterplot X axis expressed as a single column of an xts matrix
#'
#' @param y     Scatterplot Y axis expressed as a xts matrix column of the same
#'              length as x.
#'
#' @param tf    The timeframe to use for the scatterplot
#'
#' @return      This function creates a plot and does not return anything.
#'
#' @export
#------------------------------------------------------------------
splot <- function(x, y, tf = "2013/2019") {

  xymat <- xtsbind(x[tf, ], y[tf, ])
  xymat <- xymat[complete.cases(xymat), ]
  tf2   <- paste(first(index(xymat)), "to", last(index(xymat)))
  xymat <- as.matrix(xymat)

  cnames <- colnames(xymat)
  plot(xymat, main = paste(cnames[2], "vs.", cnames[1], "for\n", tf2))
  abline(lm(xymat[, 2] ~ xymat[, 1]), col = "red")  # Regression line
  abline(h = 0, col = "blue", lty = "dotted")

}

