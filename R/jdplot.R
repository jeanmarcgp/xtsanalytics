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
jdplot <- function(x, y, target, pch = 20, mode = "simple", col = c("grey"),
                   qtiles = list(top = c(0.95, 1.0), bottom = c(0, 0.05))
                   ) {

  #######  For testing  #############
  library(xtsanalytics)
  qtiles = list(top = c(0.95, 1.0), bottom = c(0, 0.05))
  mode      = "simple"
  prices    = xts_data[, c("SPY", "BND")]
  features  = make_features(prices, features = c("mom63", "sd63"), by_symbol = TRUE)
  x         = features$SPY$sd63
  y         = features$BND$sd63

  target    = lag(features$SPY[, "mom63"], k = -63)
  colnames(target) = "mom63fwd"

  pch       = 20
  col       = c("grey")

  ##################################

  xname <- names(x)
  yname <- names(y)
  zname <- names(target)

  mat1  <- as.xts(data.frame(target = target, x = x, y = y))
  colnames(mat1) <- c("target", "x", "y")
  mat1  <- mat1[complete.cases(mat1), ]
  print(tail(mat1))

  # All data regression line
  regressAll  <- lm(target ~ x + y, data = mat1)
  rsqAll      <- round(summary(regressAll)$r.squared, 3)



  #-----------------------------------------------------------
  # Create the list of quantile subsets - qsub
  # Add quantile column to select qtile rows
  #-----------------------------------------------------------
  nqtiles     <- names(qtiles)
  qsub        <- vector("list", length(qtiles))
  names(qsub) <- nqtiles
  mat1$quantnum <- 0                # 0 = no assigned quantile subset

  for(i in nqtiles) {
    phigh     <- max(qtiles[[i]])
    plow      <- min(qtiles[[i]])
    qsub[[i]] <- subset(mat1, target <= quantile(target, probs = phigh, na.rm = TRUE) &
                          target >= quantile(target, probs = plow, na.rm = TRUE))
    qindex    <- index(qsub[[i]])
    mat1[qindex, "quantnum"] <- which(nqtiles == i)

  }

  #subhi = subset(mat1, target >= quantile(target, probs = 0.95, na.rm = TRUE))
  #sublo = subset(mat1, target <= quantile(target, probs = 0.05, na.rm = TRUE))

  #---------------------------------------------------------------
  # Assign
  #---------------------------------------------------------------

  df1  <- as.data.frame(mat1)
  df1$quantile <- ifelse(df1$quantnum == 0, "None", nqtiles[df1$quantnum])
  df1$quantile <-

  plot(df1$x, df1$y, pch = pch, main = "hello",
       xlab = paste(xname, "Distribution"), ylab = paste(yname, "Distribution"),
       col = col      )

  #col = rainbow(length(df1$target), start = rainbow[1], end = rainbow[2])[rank(df1$target)],

}  ########  END FUNCTION jdplot  ##########




