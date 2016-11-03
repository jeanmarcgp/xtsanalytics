####################################################################################
# FILE acf_highmoments.R
#
#
#
####################################################################################
#
# FUNCTION acf_highmoments
#'
#' Plot the higher moment auto-correlations and their cross-correlations
#'
#' This function is useful to explore the correlation of returns within
#' a time series. A price series is given, and returns are calculated for
#' the specified periods.  Higher moments of returns are also calculated up
#' to the quintic returns (power of 5). Autocorrelations and cross-correlations
#' are then computed and plotted.
#'
#' @param prices   An xts of prices for the asset under analysis.  Only the
#'                 first column is used.
#'
#' @param on       A character vector specifying the return granularity to
#'                 calculate.  Valid granularities include any or all of:
#'                 "days", "weeks" and "months".
#'
#' @param moments  Specifies which moments of returns to calculate. The
#'                 autocorrelations and cross-correlations will be performed
#'                 for all permutations.
#'
#' @return No value is explicitly returned, but several plots are generated
#'         using the acf function from the stats package.
#'
#' @export
#-----------------------------------------------------------------------------
acf_highmoments <- function(prices, on = c('days', 'weeks', 'months'),
                            moments = c(1, 2, 3, 4, 5)) {

  prices  <- prices[, 1, drop = FALSE]     # Only process first column
  pricesw <- prices[endpoints(prices, on = "weeks"), ]
  pricesm <- prices[endpoints(prices, on = "months"), ]

  rets    <- ROC(prices,  na.pad = FALSE)
  retsw   <- ROC(pricesw, na.pad = FALSE)
  retsm   <- ROC(pricesm, na.pad = FALSE)

  #---------------------------------------
  # Plot Daily ACFs
  #---------------------------------------
  if("days" %in% on) {
    allrets <- rets_highmoments(rets, moments)
    cnames <- as.list(colnames(allrets))
    plotacf_highmoments(allrets, main = "Daily")

  }

  #---------------------------------------
  # Plot Weekly ACFs
  #---------------------------------------
  if("weeks" %in% on) {
    allrets <- rets_highmoments(retsw, moments)
    cnames <- as.list(colnames(allrets))
    plotacf_highmoments(allrets, main = "Weekly")

  }

  #---------------------------------------
  # Plot Monthly ACFs
  #---------------------------------------
  if("months" %in% on) {
    allrets <- rets_highmoments(retsm, moments)
    cnames <- as.list(colnames(allrets))
    plotacf_highmoments(allrets, main = "Monthly")

  }

}   ### END Function acf_highmoments


#-----------------------------------------------------------
# Helper function rets_highmoments
# Used to compute returns up to ^5
# NOT EXPORTED
#-----------------------------------------------------------
rets_highmoments <- function(rets, moments) {

  nc       <- length(moments)
  allrets  <- xts(matrix(rep(rets, nc), ncol = nc), order.by = index(rets))
  cnames   <- paste0(colnames(rets)[1], "^", moments)
  colnames(allrets) <- cnames

  for(i in 1:nc) {
    allrets[, i] <- allrets[, i]^moments[i]
  }

  return(allrets)

}


#-----------------------------------------------------------
# Helper function plotacf_highmoments
# Used to plot acf and cross correlations of high moments
# NOT EXPORTED
#-----------------------------------------------------------
plotacf_highmoments <- function(allrets, main) {

  cnames <- paste0(main, "_", colnames(allrets))
  colnames(allrets) <- cnames

  nc <- ncol(allrets)

  # Autocorrelations and all cross-correlations
  for(i in 1:nc) {
    for(j in i:nc) {
      #sprint("acf of returns in columns[%s, %s]", i, j)
      if(i == j) acf(allrets[, i], main = colnames(allrets[, i])) else
        acf(allrets[, c(i, j)])

    }  ### end j loop
  }    ### end i loop

}  ### END Function
