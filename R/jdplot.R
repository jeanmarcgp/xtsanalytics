#
#  FUNCTION  jdplot.R
#  ---------------------
#
#' Function to visualize the predictive power of a joint distribution
#'
#' This function is used to gain insights into a joint distribution {x, y} and its
#' predictive power over the third variable, the target.
#'
#' The function generally performs a scatterplot of {x, y} and uses color to
#' project the target variable onto the 2-D scatterplot.  The projection is at least
#' a quantile subset of the data.  In addition, the quantile subset may also be
#' grouped in clusters, each one of a given color, in order to better visualize the
#' clustering effect within that quantile.
#'
#'
#' @param x, y       The x and y data features.  These are normally 2 columns from an xts matrix.
#'
#' @param target     The target variable, also typically from an xts matrix column.
#'
#' @param mode       The plotting mode.  Supported modes are 'simple' for a simple scatterplot
#'                   to highlight the quantile regions using a given pch character and a given
#'                   color.
#'
#' @param quantiles  A named list containing the quantiles to analyze.  The quantile names can be
#'                   anything meaningful to identify the quantiles.  Each quantile is assigned a
#'                   vector of length 2, containing the bottom and upper limit of the quantile.
#'                   These limits are expressed as a number between  0 and 1.  For example,
#'                   top = c(0.95, 1.0) is the top 95% quantile and above.
#'
#' @param pch        The print characters used to generate the scatterplot.  The first
#'                   is for all points not in any quantile subset.  The next is associated to the
#'                   first quantile subset and so on.  Recycled if not enough are provided.
#'
#' @param col        The color for each data point.
#'
#' @param cex.points The cex size for the plotted points.
#'
#' @export
#----------------------------------------------------------------------------------------
jdplot <- function(x, y, target, quantiles = list(top = c(0.95, 1.0), bottom = c(0, 0.05)),
                   mode = "simple") {

  #######  For testing  #############
  library(xtsanalytics)
  quantiles = list(top = c(0.95, 1.0), bottom = c(0, 0.05))
  mode      = "simple"
  prices    = xts_data[, c("SPY", "BND")]
  features  = make_features(prices, features = c("mom63", "sd63"), by_symbol = TRUE)
  x         = features$SPY$sd63
  y         = features$BND$sd63

  target    = lag(features$SPY[, "mom63"], k = -63)
  colnames(target) = "mom63fwd"

  ##################################







}  ########  END FUNCTION jdplot  ##########




