#
#  FUNCTION  jdanalyze.R
#  -----------------------
#
#' Function to analyze the predictive power of a joint distribution
#'
#' This function is used to gain insights into a joint distribution {x, y} and its
#' predictive power over the third variable, the target.
#'
#' The function prepares an xts matrix to associate each vector to a quantile,
#' based upon a set of quantile specifications.
#'
#'
#'
#'
#' As an additional option, the quantile subsets may also be grouped into
#' clusters.  Each such cluster should be given a unique color and/or point character (pch)
#' for easy identification.  This is specified in a dataframe in argument qstyle.
#'
#' A shortcut is provided to get top and bottom quantiles at a given percentage.  The default is
#' set to Top = 95% and above and Bottom = 5% and below.  These can easily be modified using the
#' qsize argument.
#'
#' @param target, x, y  These are the 3-D data points to perform the scatter plot.  X and y
#'                      are the scatterplot x and y coordinates respectively, whereas target is
#'                      the third dimension that is expressed as a pch and/or color on the 2D
#'                      scatterplot based on its quantile / cluster.  There are two ways to
#'                      specify this data:  either each of target, x and y are single column
#'                      xts matrices, or a single 3 column xts matrix is provided as target and
#'                      x and y are NULL.
#'

#'
#' @param window    The window width to perform the rollapply to compute the rolling quantile.
#'                  If left to NULL, then no windowing is performed and the quantiles are computed
#'                  from the entire data set.
#'
#' @param qtiles    A named list containing the quantiles to analyze and a cutoff threshold used
#'                  in rolling window mode.  The cutoff thresholds are applicable only to Top and
#'                  Bottom quantiles and ignored otherwise. All other quantile names can be
#'                  anything meaningful to identify the quantiles, except
#'                  that Top and Bottom are reserved for Top and Bottom quantiles respectively.
#'                  Each quantile is assigned a vector length of either 2 or 3, where the first
#'                  two values express the bottom and upper limit of the quantile.
#'                  These limits are expressed as a number between  0 and 1.  In cases where
#'                  a third argument is provided (in rolling window mode), it is a percentage
#'                  value (expressed as a fraction between -1 and +1) above or below which the
#'                  data vector is either accepted or rejected in the quantile.  For example, if
#'                  Top is set between 0.75 and 1.0, with a window length of 100, then we would
#'                  normally expect the top 25 points in that quantile.  By adding the threshold,
#'                  then only the top 25 points that are also above that threshold are reported.
#'                  For example, during a bear market lasting the entire window, it's possible
#'                  that only a few or even no vectors exist in that quantile that also exceed
#'                  10 percent. So a cutoff threshold of 10pct would reject most (or all)
#'                  vectors in such a case.
#'
#' @param qsize     If specified, this modifies the qtiles 'Top' and 'Bottom' arguments to
#'                  the specified percentage.  qsize should be specified as a decimal i.e. 0.10
#'
#'
#' @export
#----------------------------------------------------------------------------------------
jdplot <- function(target, x = NULL, y = NULL, window = NULL, qsize = NULL,
                   qtiles    = list(Top = c(0.90, 1.0, 0.01), Bottom = c(0, 0.10, -0.01))
                   ) {

  # #######  For testing  #############
  # library(xtsanalytics)
  # library(scales)
  # window    = 252 * 0.25
  # #window    = NULL
  # qtiles    = list(Top = c(0.90, 1.0, 0.10), Bottom = c(0, 0.10, -0.10))
  # mode      = "simple"
  # mtitle    = NULL
  # qsize     = 0.2
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
  # Arguments conditioning:  x, y, qtiles and mtitle
  #--------------------------------------------------------------
  if(is.null(x) && is.null(y)) {
    x      <- target[, 2]
    y      <- target[, 3]
    target <- target[, 1]
  }

  if(!is.null(qsize)) {
    qtiles[["Top"]][1]     <- 1 - qsize
    qtiles[["Bottom"]][2]  <- qsize
  }


  xname <- names(x)
  yname <- names(y)
  zname <- names(target)


  mat1  <- as.xts(data.frame(target = target, x = x, y = y))
  colnames(mat1) <- c("target", "x", "y")
  mat1  <- mat1[complete.cases(mat1), ]


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
    phigh     <- max(qtiles[[i]][1:2])
    plow      <- min(qtiles[[i]][1:2])

    if(is.null(window)) {
      #----------------------------------------------------------------
      # Quantiles on all data
      #----------------------------------------------------------------
      qsub[[i]] <- subset(mat1, target <= quantile(target, probs = phigh, na.rm = TRUE) &
                            target >= quantile(target, probs = plow, na.rm = TRUE))
      qindex    <- index(qsub[[i]])
      mat1[qindex, "quantnum"] <- which(nqtiles1 == i)
    } else {
      #----------------------------------------------------------------
      # Rolling quantile from a rolling window
      #----------------------------------------------------------------
      rmat <- rollapplyr(mat1[, 1], width = window, FUN = function(x) {
        sub_i     <- which(x <= quantile(x, probs = phigh, na.rm = TRUE) &
                             x >= quantile(x, probs = plow, na.rm = TRUE))
        if(nrow(x) %in% sub_i) quant_i <- 1 else quant_i <- 0
        # Test to see if our point (last(x) meets the cutoff threshold
        # and set quant_i = 0 if it does not.
        if(i == "Top" && last(x)    < qtiles[[i]][3]) quant_i <- 0
        if(i == "Bottom" && last(x) > qtiles[[i]][3]) quant_i <- 0
        return(quant_i)
      })

      # qindex are the vectors in i  i.e. Top, Bottom, etc.
      qindex <- index(rmat[rmat[, 1] == 1])

      # Assign the right quantnum to the qindex vectors
      mat1[qindex, "quantnum"] <- which(nqtiles1 == i)

    }

  }  #####  END for loop  #######

  #------------------------------------------------------------------
  # Assign $quantile name to each subset.
  #------------------------------------------------------------------
  mat1         <- mat1[complete.cases(mat1), ]
  tf1          <- paste(index(first(mat1)), "/", index(last(mat1)))
  df1          <- as.data.frame(mat1)

  #df1$quantile <- unlist(lapply(df1$quantnum, function(x) nqtiles1[x]))


  #------------------------------------------------------------------
  # Compute alpha transparency for each point style and plot
  #------------------------------------------------------------------
  qstyle$alphacol <- NA
  for(i in 1:nrow(qstyle))
    qstyle$alphacol[i] <- alpha(qstyle$col[i], qstyle$alpha[i])

  plot(df1$x, df1$y, pch = qstyle$pch[df1$quantnum], main = mtitle,
       xlab = paste(xname, "Distribution"), ylab = paste(yname, "Distribution"),
       col = qstyle$alphacol[df1$quantnum], sub = tf1) #, ... )

  legend(x = legendloc, legend = paste(nqtiles1, "quantile", c("", qtiles)),
         col = qstyle$alphacol, pch = qstyle$pch)



}  ########  END FUNCTION jdplot  ##########

