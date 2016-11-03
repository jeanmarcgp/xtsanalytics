
####################################################################################
# FILE maxscreen.R
#
#
#
####################################################################################
#######################################################
#
# OBSERVATIONS
# ============
#  . Improve by adjusting the maxbox by volatility of the asset
#    This would like the position in IXIC in late 1999 for example.
#  . SHY has a high Sharpe ratio but low return.
#
#  . FIRST RULE: maxbox screen can first adjust the asset max
#    based on recent volatility.  this will lower the max exposure
#    during speculative periods for that asset.
#
#  . SECOND RULE: absmom. The pre-screen outputs a maxbox xts.  It can
#    use the FIRST rule max box (adjust for speculation) as a first
#    step.  In addition, it can zero out those assets that don't meet
#    a minimum annualized rolling momentum (absolute momentum) to
#    ensure the universe only contains momentum assets.  An exception
#    needs to be made for SHY or VSGBX as the cash position.
#
#  . THIRD RULE: The assets meeting the absolute momentum threshold can
#    also be ranked by their momentum.  The top K are taken.  K can be
#    the same as N (total assets in universe), in which case all assets
#    passing the absmom threshold are kept.  Alternatively, K < N so
#    as to keep the universe a manageable number of assets.
#
#
#  LAST, a write a short script to analyze the long term correlation of
#  various assets:  1mo, 3mo, 1yr, 3yrs, 5yrs.  Compute the rolling mom,
#  then the correlation of said asset vs. SP500.  Identify a basket of
#  assets with good long term returns AND low correlations.  XLE, VGPMX?
#
###################################################################
#
#' Volatility adjusted momentum screen for optimizing portfolios
#'
#' @param prices      An xts matrix of asset prices.
#'
#' @param maxweights  A vector containing the maximum allowable weight
#'                    for each asset. The vector is recycled or truncated
#'                    if too short or too long.  Default is 0.5.
#'
#' @param volfeature  The volatility feature that will be computed from
#'                    the prices matrix using function make_feature.
#'                    Default is sd252.
#'
#' @param volthresh   The volatility threshold at which the asset weight
#'                    will be scaled back, according to the formula
#'                    min(1, volthresh / asset_volatility), where
#'                    asset_volatility is the result of the volfeature
#'                    feature.
#'
#' @param cashasset   The column number or name of the cash asset.  This
#'                    asset has a fixed weight of 1 so the optimizer can
#'                    always select it, if all other assets have a zero
#'                    weight.  Default is column 1.
#'
#'
#' @return Returns an xts matrix of asset weights. Some weights may be
#'         zero if the asset was screened out at the given time index.
#'
#' @export
#--------------------------------------------------------------------
maxscreen <- function(prices, maxweights = 0.5, volfeature = "sd252",
                      volthresh = 0.12, momfeature = "mom252",
                      momthresh = 0.05, cashasset = 1) {

  # #####
  # library(xtsanalytics)
  # prices     = xts_data[, c(1:2, 4:6)]   # ensure no NAs in prices
  # maxweights = c(0.5, 0.4, 0.3, 0.4, 0.6)
  # volfeature = "sd63"
  # volthresh  = 0.12
  # momfeature = "mom252"
  # momthresh  = 0.13
  # cashasset  = 1
  #
  # symbols    = c("VSGBX", "VUSTX", "VGPMX", "SPY", "VWIGX", "VTRIX",
  #                "VWEHX", "VEIEX", "IXIC", "FSCHX")
  # prices     = mget_symbols(symbols, src = 'database', filepath = "../../Investing/DATABASE/data")
  # tf         = "1995/2016"
  # prices     = prices[tf, ]
  # #######

  rets       <- ROC(prices, type = "discrete")
  maxweights <- recycle(maxweights, ncol(prices))
  volmat     <- sqrt(252) * make_features(prices, features = volfeature)[[1]]
  volwts     <- emptyxts(cnames = colnames(volmat), rowfill = maxweights,
                         order.by = index(prices))

  #-------------------------------------------------------------------
  # Reduce each weight in volwts if volatility exceeds volthresh
  #-------------------------------------------------------------------
  normwts    <- volwts
  #x          <- apply(volmat, 2, function(x) {sapply(x, function(y) min(1, volthresh / y))})
  normwts[]  <- apply(volmat, 2, function(x) {sapply(x, function(y) min(1, volthresh / y))})
  volwts2    <- volwts * normwts

  #-------------------------------------------------------------------
  # Calculate normalized annualized momentum and zero out weights
  # that don't meet the volthres minimum (absolute momentum).
  #-------------------------------------------------------------------
  featnum    <- as.numeric(stringr::str_extract(momfeature, "[[:digit:]]+"))
  scfactor   <- featnum / 252
  mommat     <- make_features(prices, features = momfeature)[[1]] ^ scfactor
  mommat     <- na.locf(mommat, na.rm = TRUE)

  volwts2    <- volwts2[index(mommat), ]
  absmom_b   <- ifelse(mommat > momthresh, 1, 0)

  volwts3    <- volwts2
  volwts3[]  <- volwts2 * (absmom_b)
  volwts3[]  <- ifelse(volwts3 < 0.01, 0.0, volwts3)  # Reduce residuals to 0.0

  #------------------------------------------------------------------
  # Override the maximum weight for the cash asset to 1.0, to
  # provide a default asset if everything else is screened out.
  #------------------------------------------------------------------
  volwts3[, cashasset] <- 1


  return(volwts3)

}
