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
#' project the target variable onto the 2-D scatterplot.  The projection should at
#' least contain one quantile region of the data, thus splitting the data into
#' at least two subsets, the quantile region and all other points.
#'
#' As an additional option, the quantile subsets may also be grouped into
#' clusters.  Each such cluster should be given a unique color and/or point character (pch)
#' for easy identification.  This is specified in a dataframe in argument qstyle.
#'
#'
#' @param target, x, y  These are the 3-D data points to perform the scatter plot.  X and y
#'                      are the scatterplot x and y coordinates respectively, whereas target is
#'                      the third dimension that is expressed as a pch and/or color on the 2D
#'                      scatterplot based on its quantile / cluster.  There are two ways to
#'                      specify this data:  either each of target, x and y are single column
#'                      xts matrices, or a single 3 column xts matrix is provided as target and
#'                      x and y are NULL.
#'
#' @param mode      The plotting mode.  Supported modes are 'simple' for a simple scatterplot
#'                    to highlight the quantile regions using a given pch character and a given
#'                                       color.
#' @param qtiles    A named list containing the quantiles to analyze.  The quantile names can be
#                   anything meaningful to identify the quantiles.  Each quantile is assigned a
#                   vector of length 2, containing the bottom and upper limit of the quantile.
#                   These limits are expressed as a number between  0 and 1.  For example,
#                   top = c(0.95, 1.0) is the top 95% quantile and above.
#
#' @export
#----------------------------------------------------------------------------------------
jdplot <- function(target, x = NULL, y = NULL, mode = "simple",
                   qtiles    = list(Top = c(0.95, 1.0), Bottom = c(0, 0.05)),
                   qstyle    = data.frame(qnames = c("Other", "Top", "Bottom"),
                                          col    = c("grey20", "green", "red"),
                                          pch    = c(  20,       8,       5),
                                          alpha  = c( 0.15,      1,       1)),
                   legendloc = "topleft", mtitle = NULL, ... ) {

  # #######  For testing  #############
  # library(xtsanalytics)
  # library(scales)
  # qtiles    = list(Top = c(0.95, 1.0), Bottom = c(0, 0.05))
  # mode      = "simple"
  # mtitle    = NULL
  # legendloc = "topleft"
  # qstyle    = data.frame(qnames = c("Other", "Top", "Bottom"),
  #                        col    = c("grey20", "green", "red"),
  #                        pch    = c(  20,       8,       5),
  #                        alpha  = c( 0.15,      1,       1))
  #
  # prices    = xts_data[, c("SPY", "BND")]
  # features  = make_features(prices, features = c("mom63", "sd63"), by_symbol = TRUE)
  # x         = features$SPY$sd63
  # y         = features$BND$sd63
  # target    = lag(features$SPY[, "mom63"], k = -63)
  # colnames(target) = "SPY_mom63fwd"
  # colnames(x)      = "SPY_sd63"
  # colnames(y)      = "BND_sd63"
  #
  # ##################################

  #--------------------------------------------------------------
  # Arguments conditioning
  #--------------------------------------------------------------
  if(is.null(x) && is.null(y)) {
    x      <- target[, 2]
    y      <- target[, 3]
    target <- target[, 1]
  }

  xname <- names(x)
  yname <- names(y)
  zname <- names(target)

  if(is.null(mtitle)) mtitle <- paste0("Target (", zname, ") vs. Joint ",
                                       xname, " & ", yname, " Distribution")

  mat1  <- as.xts(data.frame(target = target, x = x, y = y))
  colnames(mat1) <- c("target", "x", "y")
  mat1  <- mat1[complete.cases(mat1), ]

  # All data regression line
  regressAll  <- lm(target ~ x + y, data = mat1)
  rsqAll      <- round(summary(regressAll)$r.squared, 3)

  #-----------------------------------------------------------
  # Create the list of quantile subsets - qsub
  # Add quantile column to select qtile rows
  #-----------------------------------------------------------
  nqtiles     <- names(qtiles)
  nqtiles1    <- c("Others", nqtiles)
  qsub        <- vector("list", length(qtiles))
  names(qsub) <- nqtiles
  mat1$quantnum <- 1                # 1 = no assigned quantile subset

  for(i in nqtiles) {
    phigh     <- max(qtiles[[i]])
    plow      <- min(qtiles[[i]])
    qsub[[i]] <- subset(mat1, target <= quantile(target, probs = phigh, na.rm = TRUE) &
                          target >= quantile(target, probs = plow, na.rm = TRUE))
    qindex    <- index(qsub[[i]])
    mat1[qindex, "quantnum"] <- which(nqtiles1 == i)

  }

  #------------------------------------------------------------------
  # Assign $quantile name to each subset.
  #------------------------------------------------------------------
  df1          <- as.data.frame(mat1)
  df1$quantile <- unlist(lapply(df1$quantnum, function(x) nqtiles1[x]))

  qstyle$alphacol <- NA
  for(i in 1:nrow(qstyle))
    qstyle$alphacol[i] <- alpha(qstyle$col[i], qstyle$alpha[i])

  plot(df1$x, df1$y, pch = qstyle$pch[df1$quantnum], main = mtitle,
       xlab = paste(xname, "Distribution"), ylab = paste(yname, "Distribution"),
       col = qstyle$alphacol[df1$quantnum], ... )

  legend(x = legendloc, legend = paste(nqtiles1, "quantile", c("", qtiles)),
         col = qstyle$alphacol, pch = qstyle$pch)



}  ########  END FUNCTION jdplot  ##########




