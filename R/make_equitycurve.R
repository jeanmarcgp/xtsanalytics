####################################################################################
# FILE make_equitycurve.R
#
#
####################################################################################
# FUNCTION make_equitycurve
#
#' Builds equity curves based on a daily timer and a set of rules
#'
#' An xts matrix of an asset price and a binary timer is provided, along
#' with a list of rules for converting the daily binary timer into equity
#' curves. Each list objet in the set of rules corresponds to an equity curve.
#' Both long only
#' and long-short equity curves are returned.
#'
#' Supported rules include perioding endpoints with optional offsets,
#' and a running sum of positives over a sliding window (sma rule), where the
#' thresholds for an up or down call can be set independently enabling to implement
#' hysteresis.
#'
#'
#' NOTE:  Trinary timers are not yet supported.
#'
#' @param data    An xts matrix consisting of 2 columns.  The asset prices in the first
#'                column and an associated timer in the second column.
#'                The timer may be binary or trinary.  If more
#'                than two columns are provided, only the first two are used.
#'
#' @param rules   A list of rules associated with each equity curve to generate
#'                from the daily timer.  Each list
#'                object consist of a named list which names the equity curve and
#'                contains the rules to construct that equity curve.  The first
#'                item on each named list must be the name of the ruleset to follow,
#'                and other items are its arguments, which may be optional. Currently
#'                supported rules include the following:
#'                \itemize{
#'                  \item \strong{"endpoints"} Uses the endpoints function to generate
#'                     the equity curve.  Optional additional arguments include the value
#'                     for on (default = "weeks"), and the value for k (default = 1).  See
#'                     function xts::endpoints for details.
#'                   \item \strong{"runsum"} NOT IMPLEMENTED - applies a running sum function (using rollapplyr)
#'                     to count the number or occurences of a state before deciding which
#'                     state to apply.  If the summation count is not met, then the previous
#'                     value stays, thereby implementing memory and hysteresis if the thresholds
#'                     are properly chosen.  Additional optional arguments are as follows, in
#'                     order: up, an integer counting the number of the
#'                   \item \strong{"sma"} implements a rolling mean (an SMA) over a window
#'                     on the binary timer, so the real valued SMA will be a number between
#'                     0 and 1 inclusive. Parameters include window specifying the
#'                     rolling window size (default = 10), up, specifying the sma threshold to
#'                     declare a timer value of 1 (default = 0.5), and dn, which specifies
#'                     the sma threshold to declare a timer value of 0 (default = 0.5). It is
#'                     possible to implement hysteresis by setting a value of up that is higher
#'                     than the value for dn.  If the value of up is LESS than dn, then the
#'                     system is unstable and oscillation will result.
#'                     }
#' @param Nlag    Number of days to lag the timer.  Default is 1.
#'
#' @return Returns a list containting xts matrices of the timers, a long only equity
#'         curve and a long-short equity curve.
#' \describe{
#'   \item{\preformatted{$long}}{
#'      An xts matrix containing the equity curves associated with each timer method,
#'      computed daily, and long when the post-processed timer is positive, or
#'      in cash otherwise (without earning any interest). The normalized asset
#'      price equity curve is also included to enable an easy comparison.
#'   }
#'   \item{\preformatted{$longshort}}{
#'      An xts matrix containing the equity curves associated with each timer method,
#'      computed daily, and long when the post-processed timer is positive, or
#'      short otherwise. The normalized asset price equity curve is also included
#'      to enable an easy comparison.
#'   }
#'   \item{\preformatted{$timers}}{
#'      An xts matrix containing the binary timers associated with each timer rule,
#'      computed daily.
#'   }
#' }
#'
#' @export
#--------------------------------------------------------------------------------
make_equitycurve <- function(data, rules, Nlag = 1) {

  datanames   <- colnames(data)
  timers      <- data[, 1]
  ecnames     <- names(rules)
  timernames  <- paste0("timer_", ecnames)

  #----------------------------------------------------------
  # TIMER LOOP: Loop through each desired equity curve and
  # process according to associated rules to get its timer.
  #----------------------------------------------------------
  for(i in 1:length(ecnames)) {
    args <- rules[[i]]
    switch(args$type,
           endpoints = {
             # Set to a default if an argument not specified
             if(is.null(unlist(args$on)))     args$on     <- "months"
             if(is.null(unlist(args$k)))      args$k      <- 1
             if(is.null(unlist(args$offset))) args$offset <- 0

             on_ep_i <- endpoints(timers, on = args$on, k = args$k) + args$offset
             on_ep_l <- on_ep_i > 0 & on_ep_i <= nrow(timers)
             on_ep   <- index(timers[on_ep_i[on_ep_l], ])

             # Create Long only timer
             timers$temp        <- NA              # create a new timer column
             timers$temp[on_ep] <- data[on_ep, 2]  # copy data timer value at endpoints
             timers$temp        <- na.locf(timers$temp, na.rm = FALSE)  # fill out
             colnames(timers)[colnames(timers) == "temp"] <- timernames[i]

           },
           sma       = {
             # Set to a default if an argument not specified
             if(is.null(unlist(args$window))) args$window  <- 10
             if(is.null(unlist(args$up)))     args$up      <- 0.5
             if(is.null(unlist(args$dn)))     args$dn      <- 0.5

             # Compute the SMA of the timer
             temp         <- TTR::SMA(data[, 2], args$window)
             timers$temp  <- ifelse(temp >= args$up, 1, NA)
             timers$temp  <- ifelse(temp <= args$dn, 0, timers$temp)
             timers$temp        <- na.locf(timers$temp, na.rm = FALSE)  # hysteresis

             colnames(timers)[colnames(timers) == "temp"] <- timernames[i]

           },
           runsum    = {
             sprint("nothing happening here...")
           }, {
             sprint("make_equitycurve:  Unknown rule specified.", rules[[i]])
             stop("Check rules argument!!!")

           })  ######  END switch statement  ######

    #---------------------------------------------
    #---------------------------------------------

  }  #####  END TIMER LOOP  #####

  #--------------------------------------------------------
  # Lag each timer by Nlag days
  #--------------------------------------------------------
  nc              <- ncol(timers)
  timers[, 2:nc]  <- xts::lag.xts(timers[, 2:nc], k = Nlag, na.pad = TRUE)

  #--------------------------------------------------------
  # Create long-short timer matrix
  #--------------------------------------------------------
  timers_LS    <- timers
  timers_LS[]  <- apply(timers, 2, function(x) ifelse(x == 0, -1, x))

  #--------------------------------------------------------
  # Build returns & ec matrix associated with each timer
  #--------------------------------------------------------
  rets       <- TTR::ROC(data[, 1], type = "discrete")
  allrets    <- xts(matrix(rep(as.numeric(rets), nc), ncol = nc), order.by = index(rets))
  allrets[, 2:nc]   <- apply(timers[, 2:nc], 2, function(x) as.numeric(rets) * x)
  colnames(allrets) <- c(datanames[1], timernames)

  allretsLS         <- allrets
  allretsLS[, 2:nc] <- apply(timers_LS[, 2:nc], 2, function(x) as.numeric(rets) * x)

  long       <- cumprod_na(1 + allrets)
  longshort  <- cumprod_na(1 + allretsLS)

  #--------------------------------------------------------
  # Return a list of all quantities
  #--------------------------------------------------------
  retlist <- list(long        = long,
                  longshort   = longshort,
                  timers      = timers)

  return(retlist)

}
