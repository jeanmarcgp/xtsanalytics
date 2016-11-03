####################################################################################
# FILE make_featurecurve.R
#
#
####################################################################################
# FUNCTION make_featurecurve
#
#' Convert a matrix of features into a corresponding set of equity curves
#'
#' This function computes matrices of equity curves associated with
#' each feature provided, according to predefined rules. A list of xts
#' matrices is returned, with each list item defined as follows:
#' \itemize{
#'    \item item \strong{$long} calculates the equity curve in a long only mode
#'    \item item \strong{$longshort} calculates the equity curves in a
#'    long-short mode
#'    }
#'
#' See Value below for details on how these equity curves are calculated.
#' This function can be useful to explore how features related to actual
#' returns.  Lagging of features is specified by argument Nlags.
#'
#' @param price       An xts matrix containing daily prices of the asset used
#'                    to build the equity curves.  Only the first column is
#'                    used.
#'
#' @param featuremat  A daily xts matrix containing the feature vectors.
#'                    The equity curves returned by this function will
#'                    map directly to this matrix, where each feature will
#'                    have its associated equity curve calculated.
#'
#' @param offsets     A \strong{list} containing numeric offsets for the
#'                    named features.  The format is <feature name> = <offset>.
#'                    If a feature is not named in that list, then its offset
#'                    defaults to zero.  This offset list is often used with
#'                    an offset of one to offset features that are centered
#'                    around one (such as ratios). This way, negative feature
#'                    value can be used to represent going short or to cash
#'                    when computing the equity curve.
#'
#' @param Nlags       The number of periods (days), either positive or negative, by
#'                    which each specified feature will be lagged. Vector recycling
#'                    is performed if length(Nlags) < number of features.  Default
#'                    is zero, recycled so no feature is lagged.
#'
#' @return  A list containing xts matrices is returned, organized as follows:
#' \describe{
#'   \item{\preformatted{$long}}{
#'      An xts matrix containing the equity curves associated with each feature,
#'      computed daily, and long when the feature value is positive or zero, or
#'      in cash otherwise (without earning any interest). The normalized asset
#'      price equity curve is also included to enable an easy comparison.
#'   }
#'   \item{\preformatted{$longshort}}{
#'      An xts matrix containing the equity curves associated with each feature,
#'      computed daily, and long when the feature value is positive or zero, or
#'      short otherwise. The normalized asset price equity curve is also included
#'      to enable an easy comparison.
#'   }
#' }
#'
#' @export
#-------------------------------------------------------------------------------------
make_featurecurve <- function(price, featuremat, offsets = NA, Nlags = 0) {

  #-------------------------------------------------
  # Lag features as specified
  #-------------------------------------------------
  fmat        <- featuremat
  featnames   <- colnames(fmat)
  nfeat       <- length(featnames)

  Rlags       <- recycle(Nlags, nfeat)

  # Lag each feature as specified by Nlags
  for(i in 1:nfeat) {
    fmat[, i] <- xts::lag.xts(fmat[, i], k = Rlags[i])
  }

  offset_vec  <- unlist(offsets)
  rets        <- TTR::ROC(price[, 1], type = "discrete")

  #-------------------------------------------------
  # Apply the offsets to the feature matrix
  #-------------------------------------------------
  ovec        <- rep(0, nfeat)
  names(ovec) <- featnames
  ovec[names(offset_vec)] <- offset_vec

  for(i in 1:nfeat) {
    fmat[, i] <- fmat[, i] + ovec[i]
  }

  #print(summary(fmat))

  #-------------------------------------------------
  # Create the timers and equity curves
  #-------------------------------------------------
  timers         <- fmat
  timers[]       <- apply(fmat, 2, function(x) ifelse(x >= 0, 1, 0))
  long_rets      <- timers
  long_rets[]    <- apply(timers, 2, function(x) ifelse(x == 1, rets[, 1], 0))
  longshort_rets <- timers
  longshort_rets <- apply(timers, 2, function(x) ifelse(x == 1, rets[, 1], -rets[, 1]))

  long_rets      <- xts::merge.xts(rets[, 1], long_rets,      join = "inner")
  longshort_rets <- xts::merge.xts(rets[, 1], longshort_rets, join = "inner")

  long_ec        <- cumprod_na(1 + long_rets)
  longshort_ec   <- cumprod_na(1 + longshort_rets)

  results <- list(long            = long_ec,
                  longshort       = longshort_ec,
                  long_rets       = long_rets,
                  longshort_rets  = longshort_rets,
                  timers          = timers
                  )

  return(results)

}



