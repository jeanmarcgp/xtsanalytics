
####################################################################################
# FILE riskparity_regularize.R
#
#
# TODO
#  .function portfolio_cor to compute the correlation of assets vs. EW portfolio
#  .
####################################################################################
#
#' Screen assets and adjust max portfolio weights using risk parity regularization
#'
#' This function is used to screen assets from a universe and adjust
#' the weights of each assets according to certain risk parity rules,
#' momentum and portfolio correlation.
#'
#'
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
#' @param maxweights  A vector containing the maximum allowable weightfor each
#'                    asset as a fraction of the total portfolio. The vector
#'                    is recycled if not enough values are provided, or truncated
#'                    if it is too long.  Default is 0.5 for all assets.
#'
#' @param volfeature  The volatility feature used to calculate the risk parity
#'                    maximum weight adjustment.  Risk parity in this context is
#'                    used to equalize risk for a given asset when compared
#'                    against its stated maximum weight.  For example, if an
#'                    asset has its maxweight set to 0.3, its risk parity
#'                    threshold set to 0.10 and its volatility at a given time
#'                    is 0.15.  Then its adjusted maxweight will be
#'                    0.3 * (0.10 / 0.15) = 0.20. Default is sd126.
#'
#' @param riskparity  The volatility threshold at which the asset weight
#'                    will be scaled back, according to the formula
#'                    min(1, riskparity / asset_volatility), where
#'                    asset_volatility is the current result of the volfeature
#'                    feature, expressed as an annualized value.
#'
#' @param momfeature
#'
#' @param momthresh
#'
#'
#' @param cashasset   The column number or name of the cash asset.  This
#'                    asset has a fixed weight of 1 so the optimizer can
#'                    always select it, if all other assets have a zero
#'                    weight.  Default is column 1.
#'
#' @param Nassets     The maximum number of assets in the portfolio at any
#'                    given time, including the cash asset.  This is used
#'                    to derive a rank threshold value beyond to screen the
#'                    top assets at each WFO date.
#'
#'
#'                    to zero.  For example, if rankthres = 4, then the top
#'                    4 highest momentum asset are included at the given date
#'                    while all other assets are excluded by setting their
#'                    weights to zero.  The cash asset however is always included
#'                    and treated separately.  So in the above example, the top
#'                    4 assets are included plus the cash asset.
#'
#' @return Returns a list of two xts matrices:  the first xts "maxweights" contains
#'                 the maximum asset weights, where the screened out asset weights
#'                 are set to zero. The second xts contains the adjusted momentum scores
#'                 (annualized) according to the following regularization function:
#'
#'
#' @export
#--------------------------------------------------------------------
riskparity_regularize <- function(prices, maxweights = 0.5, volfeature = "sd126",
                                  riskparity = 0.12, momfeature = "mom252",
                                  momthresh = 0.05, cashasset = 1,
                                  Nassets = ncol(prices)) {

  ##### Code for testing function only  ####
  library(xtsanalytics)
  prices     = xts_data[, c(1:6)]   # ensure no NAs in prices
  maxweights = c(0.5, 0.4, 0.3, 0.4, 0.6)
  volfeature = "sd63"
  riskparity  = 0.12
  momfeature = "mom84"
  momthresh  = 0.13
  cashasset  = "BND"
  Nassets = 4

  #################
  # symbols    = c("GOLD",  "VUSTX", "VGPMX", "SPY", "VWIGX", "VTRIX", "VSGBX",
  #                "VWEHX", "VEIEX", "IXIC", "FSCHX")
  # prices     = mget_symbols(symbols, src = 'database', filepath = "../../Investing/DATABASE/data")
  # tf         = "2000/2016"
  # prices     = prices[tf, ]
  #
  # #########################################

  ########################################################
  ###########  NEW CODE  #################################
  ########################################################


  #-------------------------------------------------------------------
  # Remove NAs from prices and move the cashasset to column 1,
  # Compute returns and recycle maxweights if needed
  #-------------------------------------------------------------------
  prices     <- prices[complete.cases(prices), ]
  cashname   <- colnames(prices[, cashasset])
  if(is.null(cashname)) stop("Must specify cashasset with a legal column number or name")
  cnames     <- colnames(prices)
  prices     <- cbind(prices[, cashasset], prices[, colnames(prices) != cashname])

  rets       <- ROC(prices, type = "discrete")
  if(length(maxweights) == ncol(prices)) maxweights <- maxweights[colnames(prices)] else
    maxweights <- recycle(maxweights, ncol(prices))

  #-------------------------------------------------------------------
  # Compute all features and normalize to annual quantities.
  #   .Calculate the momentum feature, normalized to annual
  #   .Calculate the volatility matrix, adjust daily to annualized
  #-------------------------------------------------------------------
  momnum     <- as.numeric(stringr::str_extract(momfeature, "[[:digit:]]+"))
  momscaling <- 252 / momnum
  mommat     <- make_features(prices, features = momfeature)[[1]]
  mommat     <- (mommat + 1)^momscaling - 1
  mommat     <- na.locf(mommat, na.rm = TRUE)

  # Assume daily prices, sd features, so multiply sqrt(252) to annualize
  volmat     <- sqrt(252) * make_features(prices, features = volfeature)[[1]]
  volmat     <- na.locf(volmat, na.rm = TRUE)

  #-------------------------------------------------------------------
  # Risk Parity:
  # Reduce each weight in maxwtsmat if volatility > riskparity
  #-------------------------------------------------------------------
  riskparity <- recycle(riskparity, ncol(volmat))
  maxwtsmat  <- emptyxts(cnames = colnames(volmat), rowfill = maxweights,
                         order.by = index(volmat))  # Start with maxweights matrix

  normwts    <- maxwtsmat
  normwts[]  <- t(apply(volmat, 1, function(x) {
    ifelse(x > riskparity, riskparity / x, 1)}))
  maxwtsmat  <- maxwtsmat * normwts  # maxweights * normalized weights

  #-------------------------------------------------------------------
  # Get the WFO dates and check date ranges for at least 2 dates
  #-------------------------------------------------------------------
  tempmat <- xtsbind(maxwtsmat, mommat)
  tempmat <- tempmat[complete.cases(tempmat), ]  # to get first possible WFO date


  #-------------------------------------------------------------------

  #-------------------------------------------------------------------
  # Build the maxweight matrix  risk parity adjustment => weigthmat
  #-------------------------------------------------------------------
  #-------------------------------------------------------------------
  # Perform the absolute momentum screen => assetmat
  #-------------------------------------------------------------------
  #-------------------------------------------------------------------
  # MAIN LOOP over wfodates
  #  .Implement hysteresis via sellrank
  #  .initialize holdings <- cashasset (SHY)
  #  .For each wfodate:
  #    1) corvec <- portfolio_cor():  holdings vs. each asset
  #    2) volvec <- apply sd to each asset (computed for maxweight matrix)
  #       then normalize volvec vs. holdings volatility
  #    3) if absolutecor then abs(corvec)
  #    4) regvec <- 1 / (1 + lambda1 *(corvec + epsilon)^cnorm + lambda2 * volvec)
  #    5) regmomvec <- momvec * regvec
  #    6) rankmomvec <- rank(regmomvec)  Rank all assets not screened out.
  #   Next, sell assets not meeting ranksell
  #-------------------------------------------------------------------
  #  1.






  ########################################################





  #sprint("volwts2")
  #print(head(volwts2))



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

  #sprint("rankthres = %s", Nassets)


  #------------------------------------------------------------------------------
  # Compute absolute momentum filter and relative momentum rank filter
  # using rankthres.  relmom_b and absmom_b are booleans
  # Zero out weights that don't meet the volthres minimum (absolute momentum)
  #------------------------------------------------------------------------------
  #sprint("Now doing the ifelse statements...")

  # Relative momentum filter to select only top ranking assets incl. cash
  relmom_b   <- rankmat
  relmom_b[] <- ifelse(as.numeric(rankmat) <= Nassets, 1, 0)
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
