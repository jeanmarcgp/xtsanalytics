#
#
#  features.R
#  ----------
#
#  Functions to generate indicators and features from an xts series.
#
#  LIST OF FUNCTIONS:
#  ------------------
#  .funapply
#  .make_timer
#  .make_bench
#
#
#  .make_labels
#
#  .make.indicators     Returns an xts of a specified vector of indicators (not exported)
#
#
###################################################################################
#
#   XTS features and indicator Functions
#
###################################################################################

#----------------------------------------------------------------------------------
#  FUNCTION funapply
#
#' Apply a function to an xts using multiple rolling windows.
#'
#' Runs rollapplyr multiple times on an xts matrix to apply a function using a
#' set of different rolling window sizes.
#'
#' The data argument is assumed to be an xts matrix.  The function is any function
#' that can be used with rollapplyr. The windows argument is a vector corresponding to
#' the rolling window size passed to FUN and rollapplyr during each iteration.
#'
#' For example, if windows == c(20, 40, 100), then there will be three iterations as follows:
#' the window size will be 20 periods for the first iteration, 40 for the second iteration,
#' and 100 periods for the third iteration.
#'
#' Since rollapplyr is used, the leading rows will contain NAs up to the window size + 1.
#' Use the zoo:na.locf function to remove the leading NAs.
#'
#'
#' @param  data    A single column xts matrix containing prices or returns.
#' @param  FUN     The name of the function that will be applied.
#' @param  windows A vector of window sizes from which to loop over and pass to the
#'                 rollapplyr function at each iteration.
#' @param  ...     additional arguments passed to FUN (this is fixed for all iterations).
#' @return An xts matrix with the same number of columns as the length of the windows
#'         parameter, where each column corresponds to one of the window sizes i.e.
#'         a column is associated with an iteration.
#'
#' @export
funapply <- function(data, FUN, windows, ...) {

  stopifnot(xts::is.xts(data))
  stopifnot(ncol(data) == 1)
  stopifnot(!is.null(windows))

  fun_name <- substitute(FUN)
  FUN <- match.fun(FUN)

  x <- xts::xts(x = NULL, order.by = zoo::index(data))
  for(i in seq_along(windows)) {
    cname <- paste0(fun_name, windows[i])
    x$newcol <- zoo::rollapplyr(data[, 1] , width = windows[i], FUN = FUN, ...)
    colnames(x)[colnames(x) == 'newcol'] <- cname
  }

  return(x)

}  ######  END funapply  ######

#----------------------------------------------------------------------------------
#  FUNCTION make_timer
#
#' Create an asset timer from an indicator.
#'
#' Generate an asset timer based on a previously generated indicator
#' and either an asset return series or price series.
#'
#' An xts matrix (data) is provided containing a column of asset
#' returns (or prices) and another column with an indicator of numeric or logical
#' class.  The function examines each indicator value and compares it to the
#' threshold 'thresh' to create a series of 'timed returns'.  If the indicator value
#' is ABOVE the threshold, the timed return is the asset return multiplied by
#' the first multiplier value (default 1).  Otherwise, the timed return
#' is the asset return multiplied by the second multiplier value
#' (default 0).  See examples.
#'
#'
#' @param data        An xts series containing an indicator column and either a
#'                    prices column or a returns column.
#'
#' @param cols        A length 2 vector specifying the column names for the
#'                    returns (or prices) and the indicator respectively.  The
#'                    returns, if provided, are assumed to be simple (discrete) returns.
#'
#' @param thresh      The numeric threshold of the indicator value used to time the
#'                    asset. Default is 0. See details.
#'
#' @param mult        A length 2 numeric vector.  The first value is the multiplier
#'                    used when the indicator is ABOVE the threshold, otherwise the
#'                    second value is used.  Default c(1, 0).
#'
#' @param retval      Specifies which column(s) are included in the returned xts matrix.
#'                    This is a vector containing the desired column names to subset.
#'                    Valid strings include:  c('rets', 'timedrets', 'timer', 'timerlag',
#'                    'ec', 'timedec'), with the following effects. Default value is
#'                    'timedrets'.
#'
#'                    \itemize{
#'                      \item{'rets' and 'timedrets':  The discrete returns from the
#'                             asset (not lagged), and the timed version, lagged by one period. }
#'
#'                      \item{'timer' and 'timerlag':  The binary timers, with values as
#'                             specified by the mult parameter.  For example, if mult = c(1,0)
#'                             then the timer value is either 1 (above threshold) or 0 (less or equal to
#'                             the threshold).  'timerlag' is the lagged version
#'                             of the timer (by a single period).}
#'                      \item{'ec' and 'timedec':  The equity curve, ignoring any leading NAs.
#'                            'ec' is the equity curve from 'rets', and thus without the timer.
#'                            'timedec' is the equity curve from 'timedrets', based on the
#'                            1 period lagged timer.}
#'                    }
#'
#' @param on          The sampling period at which to compute the timer.  This value is
#'                    passed to endpoints().  Typical values
#'                    include 'days', 'weeks', 'months', 'quarters' and 'years'.
#'
#' @param offset      The number of periods to add to offset the sampling dates.
#'                    Default is 0.
#'
#' @param seriestype  Specifies the type of asset series provided to generate the timer.
#'                    Can be either 'rets', for discrete asset returns, or 'prices' for asset prices.
#'                    Default is 'rets'.
#'
#' @param vote        A length 2 numeric vector or NULL (default). This is used to provide additional
#'                    filtering of the internal timer via a form of hysteresis.  It assumes that
#'                    mult = c(1,0), the
#'                    default, otherwise some unpredictable results may occur.  The internal timer is
#'                    examined via a rolling window of size sum(vote) + 1.  The number of 1s are counted
#'                    within the window.
#'                    If the count is > vote[1], then the timer is declared to be one, otherwise it is
#'                    zero. For example, and given that mult = c(1,0), then
#'                    the internal timer is 1 when above the threshold, and 0 otherwise.  If vote = c(15,5),
#'                    then a rolling window of 15 + 5 + 1 = 21 is applied (the +1 needed to have a minimum window
#'                    size of 1).  If at least 15 observations of the
#'                    internal timer are 1, then the timer used will be one.  Otherwise, it will be 0, which
#'                    is equivalent to at least 5 observations being zero.
#'                    If NULL, then no rolling vote is applied and the timer returned is identical to
#'                    the internal timer.
#'
#'
#' @return An xts matrix with columns as specified by parameter retval.
#'
#'
#' @seealso endpoints()
#'
#' @examples
#' data <- xts_data[,1]
#' data$rets <- ROC(data[,1], type="discrete")
#' data$indicator <- funapply(data$rets, FUN=mean, windows=200)
#' x <- make_timer(data)
#' y <- make_timer(data, mult = c(1, 0), retval = c('timedrets', 'timer', 'ec', 'timedec'))
#' z <- cbind(data, y)
#' z["2014-12", ]
#' xtsplot(z[, c('ec', 'indicator_timedec')], main = "make_timer() Example")
#'
#' @export
make_timer <- function(data, cols = c('rets', 'indicator'), retval = 'timedrets',
                       thresh = 1, mult = c(1, 0),  on = 'days',
                       offset = 0, seriestype = 'rets', vote = NULL ) {

  #  Test for valid arguments
  stopifnot(xts::is.xts(data),
            length(cols) == 2,
            length(mult) == 2,
            all(retval %in% c('ec', 'timedec', 'rets', 'timedrets', 'timer', 'timerlag')),
            length(retval) > 0,
            on %in% c('days', 'weeks', 'months', 'quarters', 'years'),
            seriestype %in% c('rets', 'prices'),
            is.numeric(offset),
            is.null(vote) || (is.numeric(vote) && length(vote) == 2 && vote[1] >= 0 && vote[2] >= 0)
            )

  # Extract returns, compute equity curve
  asset <- data[, cols]
  if(seriestype == 'prices') {
    # Create a rets column
    asset$rets <- TTR::ROC(asset[, cols[1]], type = 'discrete')
    cols[1]    <- 'rets'              #  replace price for rets name, the newly created rets column
  } else {
    asset$rets <- asset[, cols[1]]    # make a copy of the rets column, named 'rets' for later
  }
  asset$ec   <- cumprod_na(1 + asset[, cols[1]])

  # Compute endpoints with offset, eliminate samples outside 1:nrow
  sample_temp     <- xts::endpoints(asset, on = on) + offset
  sample_set      <- sample_temp[sample_temp %in% 1:nrow(asset)]

  # Build the DAILY timer (timerall) AND the SAMPLED timer
  asset$timertemp  <- ifelse(asset[, cols[2]] > thresh, mult[1], mult[2])

  if(!is.null(vote)) {
    rollingvote      <- sum(vote) + 1
    asset$rollsum    <- zoo::rollapplyr(asset$timertemp, width = rollingvote, FUN = sum)
    asset$timerall   <- ifelse(asset$rollsum > vote[1], 1, 0)
  } else {
    asset$timerall   <- asset$timertemp
  }

  asset$sampled   <- asset$timerall[sample_set]
  asset$timer     <- zoo::na.locf(asset[, 'sampled'])
  asset$timerlag  <- quantmod::Lag(asset$timer, 1)
  asset$timedrets <- asset[, cols[1]] * asset$timerlag
  if(any(retval == 'timedec')) asset$timedec <- cumprod_na(1 + asset$timedrets)

  # Rename columns with _col[2] name extensions, except 'ec' column
  retdata <- asset[, retval]
  retval_ext <- paste0(cols[2], "_", retval)

  if(any(retval == 'ec'))   retval_ext[which(retval == 'ec')]   <- 'ec'
  if(any(retval == 'rets')) retval_ext[which(retval == 'rets')] <- cols[1]

  #sprint('retval_ext:  %s', retval_ext)
  #dnames <- colnames(retdata)
  #sprint('print dnames next')
  #print(dnames)
  #sprint('ok?')
  #sprint('colnames of retdata: %s', dnames)
  #sprint('done!')

  colnames(retdata) <- retval_ext


  return(retdata)

}  ######  END make_timer  ######


#----------------------------------------------------------------------------------
#  FUNCTION make_bench
#
#' Create an asset benchmark timer from a standard indicator and asset returns.
#'
#' This function is a wrapper for make_timer().  It simplifies the creation of
#' an asset benchmark timer in a simpler function call.
#'
#' The argument data contains an xts price series only. If data contains more than one
#' column, then only the first column is used.  The argument type is used to specify
#' what timer to use, as an easy to remember string.  Multiple timers can be used
#' if type is passed as a vector of strings.
#'
#' @param data       The xts matrix containing the asset price series in the FIRST column.
#' @param type       A vector of character strings specifying the type of benchmark to
#'                   calculate.  Possible values include any of the following:
#'                   \itemize{
#'                     \item{'Goldencross':  The standard asset price SMA50 / SMA200 days.}
#'                     \item{'Faber10':  The 210 days (10 months) price momentum, calculated
#'                            daily.  Normally sampled monthly at end of months, so on = 'months'
#'                            should be specified. }
#'                     \item{'Dema50':  The StormGuard DEMA 50 implementation, with the 22 multiplier
#'                            of daily returns and 0.006 offset. }
#'                     \item{'Minidipper':  Implements the timer in the Mini Dipper strategy.
#'                            A crossover of price EMA65 / SMA200. }
#'                   }
#'
#' @param retval     A vector specifying which columns to return.  Values can include
#'                   any set of:  c('timer', 'timerlag', 'ec', 'timedec', 'rets', 'timedrets').
#'                   See make_timer() retval parameter for details.
#' @param on         The period at which the timer is sampled to make its decision.
#'                   These can be one of: 'days', 'weeks', 'months' or 'quarters'.  Default
#'                   is 'days'.
#' @param offset     The number of periods to add to offset the sampling dates, as
#'                   specified by the on parameter.
#'                   Default is 0.
#'
#' @param seriestype The type of asset series data provided in column 1.  Can be either
#'                   'rets' or 'prices'. If 'rets', then discrete
#'                   returns are assumed.  If 'prices', then prices are assumed.
#'                   Default is 'prices'.
#'
#'
#' @return An xts matrix with columns specified by parameter retval.
#' @examples
#' ec <- xts_data[, 'SPY']
#' ec$Goldencross <- make_bench(xts_data[, 'SPY'], type = 'Goldencross', retval = 'timedec')
#' ec$Dema50      <- make_bench(xts_data[, 'SPY'], type = 'Dema50', retval = 'timedec')
#' ec$Faber10     <- make_bench(xts_data[, 'SPY'], type = 'Faber10', retval = 'timedec')
#' xtsplot(ec, main = 'SPY and its daily timers')
#'
#' ecm <- xts_data[, 'SPY']
#' ecm$Goldencross <- make_bench(xts_data[, 'SPY'], type = 'Goldencross', on = 'months', retval = 'ec')
#' ecm$Dema50 <- make_bench(xts_data[, 'SPY'], type = 'dema50', on = 'months', retval = 'ec')
#' ecm$Faber10 <- make_bench(xts_data[, 'SPY'], type = 'Faber10', on = 'months', retval = 'ec')
#' xtsplot(ecm, main = 'SPY and its monthly timers')
#'
#' ecset <- xts_data[, 'SPY']
#' ecset$GoldenCross <- make_bench(xts_data[, 'SPY'], type = 'goldencross', on = 'months', retval = 'ec', offset=4)
#' xtsplot(ecset, main = 'SPY and 4 day offset monthly Golden Cross')
#'
#' @export
make_bench <- function(data, type = 'Goldencross', retval = 'timedec',
                       on = 'days', offset = 0, seriestype = 'prices', ...) {

  # Test for valid arguments
  stopifnot(xts::is.xts(data),
            length(retval) > 0,
            all(retval %in% c('ec', 'timedec', 'rets', 'timedrets', 'timer', 'timerlag')),
            type %in% c('Goldencross', 'Faber10', 'Minidipper', 'Dema50'),
            seriestype %in% c('rets', 'prices'),
            is.numeric(offset),
            on %in% c('days', 'weeks', 'months', 'quarters', 'years')
            )
  # Create prices and rets columns.
  asset <- data[, 1]
  colnames(asset) <- 'prices'
  asset$rets <- ROC(asset$prices, type = "discrete")
  asset <- na.locf(asset, na.rm=TRUE)     #  remove leading NA in rets column

  # Compute the indicators based on type
  switch(type,
         Goldencross = {
           # Price-based golden cross
           gc              <- asset
           gc$sma50        <- TTR::SMA(gc$prices, 50)
           gc$sma200       <- TTR::SMA(gc$prices, 200)
           gc$Goldencross  <- gc$sma50 / gc$sma200
           retdata <- make_timer(gc, cols = c('prices', 'Goldencross'), retval = retval, thresh = 1,
                                 mult = c(1, 0), on = on, offset = offset, seriestype = 'prices', ...)
         },
         Minidipper = {
           # Price-based Mini-Dipper
           md            <- asset
           md$ema170     <- TTR::EMA(md$prices, 170)
           md$sma40      <- TTR::SMA(md$prices, 40)
           md$Minidipper <- md$sma40 / md$ema170
           retdata <- make_timer(md, cols = c('prices', 'Minidipper'), retval = retval, thresh = 1,
                                 mult = c(1, 0), on = on, offset = offset, seriestype = 'prices', ...)
         },
         Faber10 = {
           # Faber 10 months momentum, computed daily
           f10           <- asset
           f10$sma210    <- TTR::SMA(f10$prices, 210)
           f10$Faber10   <- f10$prices / f10$sma210
           retdata <- make_timer(f10, cols = c('prices', 'Faber10'), retval = retval, thresh = 1,
                                 mult = c(1, 0), on = on, offset = offset, seriestype = 'prices', ...)
         },
         Dema50 = {
           # CIMI group DEMA50 implementation using fTrading
           # This should only be done on GSPC or SPY and has been curve fitted
           d50   <- asset
           alpha <- 50
           aver  <- 50
           dema  <- fTrading::emaTA(d50$rets, 1/alpha, aver)
           dema2 <- fTrading::emaTA(dema, 1/alpha, aver)

           d50$dema50 <- 22 * dema2 + 0.006


           retdata <- make_timer(d50, cols = c('rets', 'dema50'), retval = retval, thresh = 0,
                                 mult = c(1, 0), on = on, offset = offset, seriestype = 'rets', ...)
           #xtsplot(retdata)

         },
         stop('Switch error in make_bench(): type = ', type)

         )  ####  END switch on type  ####


  return(retdata)

}  ######  END make_bench  ######



#----------------------------------------------------------------------------------
#  FUNCTION make.indicators
#
#  Returns an xts matrix of all specified indicators (transformations) on the
#  price or returns of the argument data (an xts matrix).  Time indices are as
#  specified in the data argument.
#
#  .data:        price xts of the asset
#  .indicators:  a character vector containing names and numeric arguments of
#                all indicators to generate.  Syntax of each is <name>.<num>
#                where <num> is the rolling period and <name> is the indicator
#                to compute:  {mom, sma, smap, smar, smalr, ema, emap, emar,
#                emalr, sdp, sdr, sdlr, demap, demar, demalr }.  See code.
#  .na.rm:       logical to remove leading NAs in value returned.
#
#----------------------------------------------------------------------------------
make.indicators <- function(data, indicators, na.rm=FALSE) {

  # remove redundant indicator names, if any
  indicators <- unique(indicators)

  # split the indicators vector into its character and numeric portions
  indicator.chr <- regmatches(indicators, regexpr("[a-z]+", indicators))
  indicator.num <- as.numeric(regmatches(indicators, regexpr("[0-9].*", indicators)))

  data$logrets <- TTR::ROC(data[,1], type="continuous")
  data$rets <- TTR::ROC(data[,1], type="discrete")

  #  Loop over each indicator and compute its column
  default.sel <- NULL
  for(i in 1:length(indicators)) {
    switch(indicator.chr[i],
           mom    = data$temp <- TTR::ROC(data[,1], n=indicator.num[i]),
           sma    = data$temp <- TTR::SMA(data[,1], n=indicator.num[i]),
           smap   = data$temp <- TTR::SMA(data[,1], n=indicator.num[i]),
           smar   = data$temp <- TTR::SMA(data$rets, n=indicator.num[i]),
           smalr  = data$temp <- TTR::SMA(data$logrets, n=indicator.num[i]),
           ema    = data$temp <- TTR::EMA(data[,1], n=indicator.num[i]),
           emap   = data$temp <- TTR::EMA(data[,1], n=indicator.num[i]),
           emar   = data$temp <- TTR::EMA(data$rets, n=indicator.num[i]),
           emalr  = data$temp <- TTR::EMA(data$logrets, n=indicator.num[i]),
           sdp    = data$temp <- zoo::rollapplyr(data[,1], width=indicator.num[i], FUN=sd),
           sdr    = data$temp <- zoo::rollapplyr(data$rets, width=indicator.num[i], FUN=sd),
           sdlr   = data$temp <- zoo::rollapplyr(data$logrets, width=indicator.num[i], FUN=sd),
           demap  = data$temp <- TTR::DEMA(data[,1], n=indicator.num[i]),
           demar  = data$temp <- TTR::DEMA(data$rets, n=indicator.num[i]),
           demalr = data$temp <- TTR::DEMA(data$logrets, n=indicator.num[i]),

           #  Default selection
           default.sel <- c(default.sel, indicators[i])
    )
    colnames(data)[colnames(data) == 'temp'] <- indicators[i]
  }

  ret.xts <- data[, indicators]
  if(na.rm) ret.xts <- ret.xts[complete.cases(ret.xts),]
  return(ret.xts)

}  ######  END make.indicators  ######

