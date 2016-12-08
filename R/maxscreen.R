
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
#' Returns an xts matrix of asset weights, modulated according to
#' a set of rules.
#'
#' The xts matrix returned will generally start at a later time
#' that the prices matrix since features are calculated from it.
#' Therefore, you must ensure that it properly aligns with
#' other matrices in the calling function.
#'
#' The first rule applied is...
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
#' @param rankthres   The rank value beyond which an asset weight is forced
#'                    to zero.  For example, if rankthres = 4, then the top
#'                    4 highest momentum asset are included at the given date
#'                    while all other assets are excluded by setting their
#'                    weights to zero.  The cash asset however is always included
#'                    and treated separately.  So in the above example, the top
#'                    4 assets are included plus the cash asset.
#'
#' @return Returns an xts matrix of asset weights. Some weights may be
#'         zero if the asset was screened out at the given time index.
#'
#' @export
#--------------------------------------------------------------------
maxscreen <- function(prices, maxweights = 0.5, volfeature = "sd252",
                      volthresh = 0.12, momfeature = "mom252",
                      momthresh = 0.05, cashasset = 1,
                      rankthresh = ncol(prices)) {

  # ##### Code for testing function only  ####
  # library(xtsanalytics)
  # prices     = xts_data[, c(1:2, 4:6)]   # ensure no NAs in prices
  # maxweights = c(0.5, 0.4, 0.3, 0.4, 0.6)
  # volfeature = "sd63"
  # volthresh  = 0.12
  # rankthresh = 4
  # momfeature = "mom126"
  # momthresh  = 0.13
  # cashasset  = "VSGBX"
  #
  # symbols    = c("GOLD",  "VUSTX", "VGPMX", "SPY", "VWIGX", "VTRIX", "VSGBX",
  #                "VWEHX", "VEIEX", "IXIC", "FSCHX")
  # prices     = mget_symbols(symbols, src = 'database', filepath = "../../Investing/DATABASE/data")
  # tf         = "2000/2016"
  # prices     = prices[tf, ]
  #
  # #########################################

  #-------------------------------------------------------------------
  # Remove NAs from prices (to ensure volmat is clean of NAs),
  # and move the cashasset to column 1
  #-------------------------------------------------------------------
  prices     <- prices[complete.cases(prices), ]
  cashname   <- colnames(prices[, cashasset])
  cnames     <- colnames(prices)
  prices     <- cbind(prices[, cashasset], prices[, colnames(prices) != cashname])

  rets       <- ROC(prices, type = "discrete")
  if(length(maxweights) == ncol(prices)) maxweights <- maxweights[colnames(prices)] else
    maxweights <- recycle(maxweights, ncol(prices))

  volmat     <- sqrt(252) * make_features(prices, features = volfeature)[[1]]
  volwts     <- emptyxts(cnames = colnames(volmat), rowfill = maxweights,
                         order.by = index(prices))

  #-------------------------------------------------------------------
  # Risk Parity:  Reduce each weight in volwts if vol. > volthresh
  #-------------------------------------------------------------------
  normwts    <- volwts
  normwts[]  <- apply(volmat, 2, function(x) {sapply(x, function(y) min(1, volthresh / y))})
  volwts2    <- volwts * normwts

  #sprint("volwts2")
  #print(head(volwts2))

  #-------------------------------------------------------------------
  # Calculate momentum, annualized
  #-------------------------------------------------------------------
  featnum    <- as.numeric(stringr::str_extract(momfeature, "[[:digit:]]+"))
  scfactor   <- 252 / featnum
  mommat     <- make_features(prices, features = momfeature)[[1]]
  mommat     <- (mommat + 1)^scfactor - 1
  mommat     <- na.locf(mommat, na.rm = TRUE)

  #sprint("head of mommat:")
  #print(head(mommat))

  #-------------------------------------------------------------------
  # Calculate relative momentum rank, excluding cashasset
  #-------------------------------------------------------------------
  mommat_nc  <- mommat[, colnames(mommat) != cashname]

  rankmat    <- mommat_nc
  nc         <- ncol(rankmat) + 1   # to do reverse ranking
  rankmat[]  <- t(apply(mommat_nc, 1, function(x) nc - rank(x)))

  # Add the cash asset back by forcing it to have rank 0
  rankmat    <- cbind(mommat[, cashasset], rankmat)
  rankmat[, 1] <- 0

  #sprint("rankmat:")
  #print(head(rankmat))

  #sprint("rankthres = %s", rankthresh)


  #------------------------------------------------------------------------------
  # Compute absolute momentum filter and relative momentum rank filter
  # using rankthres.  relmom_b and absmom_b are booleans
  # Zero out weights that don't meet the volthres minimum (absolute momentum)
  #------------------------------------------------------------------------------
  #sprint("Now doing the ifelse statements...")

  # Relative momentum filter to select only top ranking assets incl. cash
  relmom_b   <- rankmat
  relmom_b[] <- ifelse(as.numeric(rankmat) <= rankthresh, 1, 0)
  #sprint("relmom_b:")
  #print(head(relmom_b))

  # Absolute momentum filter to select only assets > momthresh
  absmom_b   <- mommat
  absmom_b[] <- ifelse(as.numeric(mommat) > momthresh, 1, 0)
  #sprint("absmom_b:")
  #print(head(absmom_b))

  volwts2    <- volwts2[index(mommat), ]
  #sprint("volwts2")
  #print(head(volwts2))

  #sprint("cashasset = %s", cashasset)

  # AND logical absmom * relmom to get filtered assets, then apply volwts2
  volwts3    <- volwts2
  volwts3[]  <- volwts2 * absmom_b * relmom_b

  #sprint("volwts3, before ifelse:")
  #print(head(volwts3))
  # Get rid of assets with weights < 0.01
  volwts3[]  <- ifelse(as.numeric(volwts3) < 0.01, 0.0, volwts3)  # Reduce residuals to 0.0

  #------------------------------------------------------------------
  # Override the maximum weight for the cash asset to 1.0, to
  # provide a default asset if everything else is screened out.
  #------------------------------------------------------------------
  volwts3[, cashasset] <- 1

  #------------------------------------------------------------------
  # Ensure the colnames of volwts3 are in the same order as prices
  #------------------------------------------------------------------
  volwts3    <- volwts3[, cnames]

  #sprint(">>>>>>>>>>>>>>>>>> tail of volwts3:")
  #print(head(volwts3))

  return(volwts3)

}
