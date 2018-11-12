#
#  FUNCTION  rebalance.R
#  ---------------------
#
#
#' Periodic portfolio rebalance to a given set of weights
#'
#' Periodically rebalances a portfolio to a set of weights according to a weight vector.
#'
#'
#' @param prices     An xts of asset prices from which to build the equity curve
#'                   using periodic rebalancing.
#'
#' @param weights    A list or vector of asset symbols with their associated weights.
#'                   This vector should normally sum up to one.  However, what matters
#'                   is the relative ratios between each weight, not the absolute values.
#'                   The list or vector must be named, and each name must have a
#'                   corresponding column in the prices xts matrix.
#'
#' @param on         Period on which to rebalance using function endpoints.  Valid values are:
#'                   { 'weeks', 'months', 'quarters', 'years' }.
#'
#' @param rebal_offset Number of days from which to offset the rebalance.  Default is one,
#'                     which corresponds to the day after the period endpoints.  Can only be zero or
#'                     a positive number (which rebalances later than the endpoint).
#'
#'
#' @return An xts matrix of the portfolio equity curve, rebalanced at the end of the
#'         specified trading day by argument on.  The first column contains the prices
#'         equity curve while the second column contains the daily returns of the equity
#'         curve.
#'
#'
#' @export
#----------------------------------------------------------------------------------------
rebalance <- function(prices, weights, on = 'months', rebal_offset = 1) {

  # ############################
  # library(xtsanalytics)
  # # prices = xts_data["2008/2012-12-16", 1:6]
  # # weights = list( SPY = 0.60, BND = 0.40)
  # on = "months"
  # on = "years"
  # rebal_offset = 1
  #
  # prices    <- xts_data["2008/2014-12-16", c("SPY", "BND")]
  # weights   <- list(SPY = 0.60, BND = 0.40)
  # ############################

  weights <- unlist(weights)
  prices  <- prices[, names(weights)]
  prices  <- na.locf(prices)                   # fill skipped days
  prices  <- prices[complete.cases(prices), ]  # remove leading NAs

  if(rebal_offset < 0) stop("Function rebalance:  rebal_offset must be >= 0")

  #--------------------------------------------------------------------------
  # Compute the rebalancing index baseline.
  #--------------------------------------------------------------------------
  rebal_i <- endpoints(prices, on = on)
  Nr      <- nrow(prices)

  #-------------------------------------------------------------------
  # Offset dates by adding to rebal_i to get proper rebal_dates
  # Add one to ensure no lookahead bias (at rebal_offset = 0)
  #-------------------------------------------------------------------
  rebal_ioffset  <- rebal_i + rebal_offset
  rebal_ioffset  <- rebal_ioffset[rebal_ioffset <= (Nr - rebal_offset - 1)]

  rebal_ioffset1 <- rebal_ioffset + 1
  rebal_ioffset1 <- rebal_ioffset1[rebal_ioffset1 <= Nr]

  #-------------------------------------------------------------------
  # Add another rebalance at the end if last rebalance is earlier
  # This simplifies the looping code
  #-------------------------------------------------------------------
  if(last(rebal_ioffset1) < Nr) {
    rebal_ioffset1 <- c(rebal_ioffset1, Nr)
    rebal_ioffset  <- c(rebal_ioffset,  Nr - 1)
  }

  rebal_dates    <- index(prices[rebal_ioffset,])
  rebal_dates1   <- index(prices[rebal_ioffset1, ])

  timeframe <- paste0(first(rebal_dates), "/", last(rebal_dates1))

  #-----------------------------------------------------------------------------
  # On each rebal_date and until the next one, build the normalized
  # equity curves for each asset, apply their portfolio weight during
  # the timeframe, and sum up the results to get the portfolio equity curve.
  # Then convert that EC to daily returns to build the returns for that
  # timeframe.
  #
  # .ecrets holds the equity curves normalized at each  rebal_date
  #-----------------------------------------------------------------------------
  ecrets       <- emptyxts(cnames = "ec", order.by = index(prices[timeframe, ]))

  daily_weights  <- emptyxts(cnames = names(weights), order.by = index(ecrets))
  daily_weights[1, ] <- weights
  daily_weights      <- na.locf(daily_weights)

  # If rebal_dates is NOT the last date, then need on extra iteration

  for(i in 2:length(rebal_dates)) {
    tf       <- paste0(rebal_dates[i-1], "/", rebal_dates1[i])
    tf1      <- paste0(rebal_dates1[i-1], "/", rebal_dates1[i])  # skips first day!
    x        <- xtsnormalize(prices[tf, ])
    x2       <- as.xts(t(apply(x, 1, function(z) z * daily_weights[rebal_dates[i-1], ])))
    x2$ec    <- rowSums(x2)
    partrets <- ROC(x2$ec, type = "discrete")[-1]
    ecrets[tf1, ] <- partrets
  }

  #----------------------------------------------------------------------------
  # Build the equity curve for the entire timeframe from the daily returns
  #----------------------------------------------------------------------------
  ec      <- cumprod_na(1 + ecrets)
  ec$rets <- ecrets

  #xtsplot(ec[, "ec"])

  #--------------------------------------------------------
  # Return xts for equity curve
  #--------------------------------------------------------
  return(ec)




}  ########### END FUNCTION rebalance ###########

