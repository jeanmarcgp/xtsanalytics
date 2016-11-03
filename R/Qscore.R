####################################################################################
# FILE Qscore.R
#
#
#
####################################################################################
#
# FUNCTION Qscore
#
#'  Calculate the Quality score of a prediction
#'
#'  The quality score (Qscore) is a simple measure of prediction quality
#'  of a model.  The Qscore is a performance comparison of three equity
#'  curves:  a buy and hold strategy, a long only strategy (go to cash when
#'  the prediction is down), or a long-short strategy.
#'
#'  An xts matrix of two columns must be provided.  Column 1 is the
#'  actual single period return of the asset (normally y in a model) and
#'  column 2 is the single period normalized prediction for associated time
#'  period (normally yhat in a model).
#'
#'  Normally, xy is a daily matrix, but the predictions may be done at some lower
#'  granularity (on = 'months' for example). The equity curves are built based upon
#'  the predictions (xy column 2).
#'
#'  Any rows containing NAs are removed.
#'
#'
#'
#'
#' @param xy  An xts matrix of two columns.  The first column contains the actual
#'            value (y), whereas the second column contains the predicted value (yhat)
#'            The order of the columns matter to calculate an accurate score.
#'
#' @param on  The periodic granularity of xts matrix xy, and used to accurately calculate
#'            the performance metrics.  Default is 'days'.  Can be
#'            any of: {'days', 'weeks', 'months', 'quarters' or 'years'}.
#'
#'
#' @return  The Qscore is a 5 row by 3 column data frame containing performance statistics
#'          of the three equity curves built using the prediction:  buy-hold, long-only and
#'          long-short.
#'
#' \describe{
#'    \item{The columns are:}{\preformatted{
#'       Buy_Hold:   Performance results for a buy and hold strategy of that asset.
#'
#'       Long_Short: Performance when buying the asset on a positive prediction and shorting
#'       the asset on a negative prediction.
#'
#'       Long_Only:  Performance when buying the asset on a positive prediction and staying in cash
#'       otherwise. }}
#'
#'    \item{The rows contain performance figures as follows:}{\preformatted{
#'       Annualized Return:   Annualized return expressed in percentage.
#'
#'       Maximum Drawdown:    Maximum drawdown expressed in percentage.
#'
#'       Annualized Std Dev:  Standard deviation of returns in percent, annualized.
#'
#'       Annualized Sharpe:   The annualized Sharpe ratio (risk free rate = 0%).
#'
#'       Excess Return:       The excess annualized return in percent above the Buy and Hold strategy.
#'
#'       }}
#'    }
#'
#' TODO: Add trade statistics, such as total number of trades, and average number of
#' trades per year.
#'
#' @export
#-------------------------------------------------------------------------------------
Qscore <- function(xy, on = "days") {

  if(ncol(xy) != 2) stop("Qscore:  Argument xy must have exactly two columns.")

  xy        <- xy[complete.cases(xy), ]
  N         <- nrow(xy)

  xy$pred_sign  <- ifelse(sign(xy[, 2]) == 1, 1, 0)
  xy$Buy_Hold   <- xy[, 1]
  xy$Long_Short <- ifelse(as.logical(xy$pred_sign), xy[, 1], -xy[, 1])
  xy$Long_Only  <- ifelse(as.logical(xy$pred_sign), xy[, 1], 0)


  xyperf <- perf_df(xy[, 4:6], on = on, percent = TRUE, digits = 2)

  Qscore <- cbind(xyperf[1,1, drop = FALSE] - xyperf[1,1, drop = FALSE],
                  xyperf[1, 2:ncol(xyperf)] - rep(xyperf[1, 1], ncol(xyperf) - 1))
  Qscore <- round(Qscore, 2)
  rownames(Qscore) <- "Excess Return (%)"
  xyperf <- rbind(xyperf, Qscore)

  rval <- list(Qscore = xyperf, ec = cumprod_na(1 + xy[, 4:6]))
  return(rval)

}


