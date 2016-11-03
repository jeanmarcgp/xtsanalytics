#
#  FUNCTION  rebalance.R
#  ---------------------
#
#' Periodic portfolio rebalance to a given set of weights (NEED TO TEST)
#'
#' Periodically rebalances a portfolio to a set of weights according to a weight vector.
#'
#' NOTE:  This may be buggy!  Test and examine the code first!!!
#'
#' @param prices     An xts of asset prices from which to build the equity curve
#'                   using rebalancing.
#' @param weights    A list or vector of asset symbols with their associated weights.
#'                   This vector must sum up to one.
#'
#' @param on         Period on which to rebalance using function endpoints.  Valid values are:
#'                   { 'weeks', 'months', 'quarters', 'years' }.  If on is a vector, then
#'                   multiple equity curves are produced corresponding to each value of on.
#'
#' @param offset     Number of days from which to offset the rebalance.  Default is zero,
#'                   which corresponds to the on period endpoints.  May be a positive or
#'                   negative number.
#'
#' @param normalize  Logical.  Normalizes the equity curve of each asset price before
#'                   doing a rebalance.
#'
#'
#' @return An xts matrix of the portfolio equity curve, rebalanced at the end of the
#'         specified trading day by argument on.  If argument on is a vector then there
#'         will be length(on) equity curves produced.
#'
#----------------------------------------------------------------------------------------
rebalance <- function(prices, weights, on = 'quarters', offset = 0, normalize = TRUE) {

  # ############################
  # prices = xts_data["2008/2012", 1:4]
  # weights = list(SPY = 0.2, VTI = 0.1, BND = 0.5, VNQ = 0.2)
  # on = 'days'
  # on = "months"
  # on = c('days', 'months', 'quarters')
  # normalize = TRUE
  # ###############

  weights <- unlist(weights)
  N     <- nrow(prices)
  data  <- na.locf(prices)
  if(N != nrow(data)) stop("prices XTS has some NAs.  Clean this up first before calling rebalance...")

  rets  <- ROC(data, type = "discrete")

  if(normalize) {
    data <- data[complete.cases(data), ]
    coredata(data) <- apply(data, 2, function(x) x / rep(x[1], length(x)))
  }

  #--------------------------------------------------------------------------
  # Generate all equity curves looping through on vector
  #--------------------------------------------------------------------------
  ecurves <- NULL
  for(i in on) {
    ep <- endpoints(rets, on = i)
    ep <- ep[which(ep >= 1)]
    ep_dates <- index(rets[ep, ])
    Ndates   <- length(ep_dates)

    # Convert weights to a matrix, to use for $allweights
    weightmat <- t(as.matrix(weights))

    weightlist        <- rep(list(list(weights = weights, allweights = weightmat)), Ndates)
    folio             <- list(R = rets, opt_rebalancing = weightlist)
    names(folio$opt_rebalancing) <- ep_dates
    class(folio)      <- "optimize.portfolio.rebalancing"

    sprint("Creating rebalanced portfolio on %s", i)
    ec           <- stitch_statfolio(folio, N=1)[, 1]
    colnames(ec) <- paste0("EC_", i)

    print(tail(ec))
    # rets_daily   <- sweep(rets, 2, weights, '*')
    # prets_daily  <- as.xts(apply(rets_daily, 1, sum), order.by = index(rets))
    # prets_daily  <- prets_daily[index(ec), ]
    # ec_daily     <- cumprod_na(1 + prets_daily)
    # ec$Daily     <- as.numeric(ec_daily)

    ecurves <- xtsbind(ecurves, ec)

  } #######  End For loop  #######

  #xtsplot(ecurves)

}  ########### END FUNCTION rebalance ###########

