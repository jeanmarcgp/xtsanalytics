#
###################################################################################
# FILE backtest_quality.R
#
# Function to test the quality of a price forecast
# based upon a day by day backtest over a period.
###################################################################################
#
#
#
#  RETHINK THIS FUNCTION
#  . BTQ should be the rolling window backtest quality score, which is measured
#    as the sign of ypred vs. the next month's sign.  We are comparing different
#    things because ypred may be a longer momentum, but ultimately, the goal is
#    the decision for next month's return, not just how y and ypred aligned.
#
#----------------------------------------------------------------------------------
# FUNCTION backtest_quality
#
#' Compute the quality of a price forecast using backtest statistics (EMPTY)
#'
#' This function calculates the likelihood that a price forecast will be
#' met or exceeded at the next rebalance date.
#'
#' Over a given time window, it looks at the price forecast every day and
#' determines whether that forecast was met.  The BackTest Quality factor (BTQ)
#' is therefore a number between 0 and 1 indicating the fraction of the daily
#' forecasts that resulting in an accurate prediction.
#'
#' For example, assume we have monthly (21 days) forecasts made daily
#' for the price of an asset.  This means that every day, a forecaster
#' function is predicting that the price of the asset will meet or exceed
#' its forecasted value 22 days forward.
#'
#' This function will test this over a time window (for example, 3 months or 63 days).
#' Therefore, 63 tests will be performed.  If the forecast was accurate 40 out of the
#' 63 days, then the BTQ factor is 40/63 = 63.5%
#'
#' The BTW factor is returned as a rolling window xts matrix.
#'
#' @param prices     The daily prices matrix of a set of assets (one or more).
#'
#' @param forecasts  The daily forecasted prices of the assets.  Dimensions must
#'                   match prices matrix.
#'
#' @param fperiod    The forecasted perioc in days.  For example, 21 days for a monthly
#'                   forecast.  This is used to compute the lag on prices for the comparison.
#'
#' @param window     The backtest window in days.  Default is 63 days (3 months).
#'
#' @return A list containing the following elements:
#'
#' \describe{
#'   \item{\preformatted{$BTQ:}}{
#'      An xts matrix containing the results of the rolling backtest quality window.
#'      Each value is between 0 and 1 and represents the fraction of the time the forecast
#'      was accurate over the time window ending on the current date.
#'      }
#'   \item{\preformatted{$backtest}}{
#'      A logical xts matrix containing the logical of each forecast, as they were made on
#'      the index date, peeking into the future.  If the future price does not yet exist,
#'      then NA is used.  A value of 1 / TRUE indicates that the actual asset price was
#'      equal or exceeded the forecasted price. If below, then 0 / FALSE is used.
#'   }
#' }
#'
#' @export
#----------------------------------------------------------------------------------
backtest_quality <- function(prices, forecasts, fperiod = 21, window = 63) {

}

