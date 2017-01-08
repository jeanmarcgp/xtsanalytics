
####################################################################################
# FILE portfolio_cor.R
#
####################################################################################
#
#' Calculate pairwise asset correlation vs. a portfolio
#'
#' Calculates the pairwise correlation between all assets in a universe
#' vs. a portfolio of assets.
#'
#' The portfolio must comprise assets in the universe otherwise an error
#' will result.  A named vector is return with the correlation of each
#' asset not in the portfolio vs. the portfolio.  The portfolio may be
#' built of equal weights or a set of predefined weights, as specified
#' by maxwtsvec.
#'
#' @param rets        An xts matrix of asset returns that include all assets
#'                    in the analysis universe, the latter being a superset
#'                    of the portfolio assets.  The correlation is calculated
#'                    over all returns provided by rets.
#'
#' @param portassets  A character vector containing the names of assets in
#'                    the portfolio. Must contain at least one name.
#'
#' @param maxwtsvec   Either an ordered or named numeric vector containing the
#'                    relative weight of each asset in the universe or a single
#'                    number. The sum of all weights does
#'                    not need to equal one (it would normally exceed one), as
#'                    this is used for relative weights.  If a single number
#'                    (for example set to 1) then, this result in equal weights.
#'
#' @param on          Specifies the returns period to use to calculate the
#'                    correlation.  Default is "days", but can also be "weeks"
#'                    or "months" for weekly and montly correlations.  The portfolio
#'                    is also rebalanced at that same frequency.
#'
#' @return   Returns a named vector of correlation values for each asset vs.
#'           the given portfolio.
#'
#' @export
#----------------------------------------------------------------------------------
portfolio_cor <- function(rets, portassets = colnames(rets)[1], maxwtsvec = 1,
                          on = "days") {

#   ########  For code testing  #############
#   library(xtsanalytics)
#   rets  = ROC(xts_data, type = "discrete")["2010", ]
#   portassets = c("VNQ", "EWN", "BND")
#   #maxwtsvec = 1
#
#   on = "weeks"
#  # on = "months"
# #  on = "days"
#   maxwtsvec = c(0.2, 0.3, 0.4, 0.35, 0.5, 0.6, 0.25, 0.1, 0.01)
#
#   ################################

  # Must have a portfolio of at least one asset!
  if(length(portassets) < 1) stop("portassets must contain at least one asset")

  #---------------------------------------------------------------
  # Compute the returns at proper period based on argument "on"
  #---------------------------------------------------------------
  if(!on %in% c("days", "weeks", "months")) stop("invalid 'on' argument.")

  ec   <- cumprod_na(1 + rets)
  rper <- endpoints(ec, on = on)[-1]
  ecper <- ec[rper,]
  prets <- ROC(ecper, type = "discrete")[-1, ]

  allassets <- colnames(prets)
  Uassets   <- length(allassets)   # Num. assets in Universe
  assetvec  <- allassets[!allassets %in% portassets]
  Nassets   <- length(assetvec)    # Num. assets to correlate

  maxwtsvec <- recycle_better(maxwtsvec, allassets, default = 0)


  #--------------------------------------------------------------
  # Compute portfolio equity curve (as returns series)
  #--------------------------------------------------------------
  portwts     <- maxwtsvec[portassets]    # portfolio weights
  portwtsmat  <- emptyxts(cnames = portassets, rowfill = portwts,
                          order.by = index(prets))
  portrets    <- prets[, portassets] * portwtsmat
  wtssum      <- rep(sum(portwts), length(portassets))[1]
  portrets$ec <- as.numeric(apply(portrets, 1, function(x) sum(x) / wtssum))


  #--------------------------------------------------------------
  # Loop over all assets not in porfolio to build corvec,
  # the named vector of asset correlations
  #--------------------------------------------------------------
  corvec        <- rep(NA, Nassets)
  names(corvec) <- assetvec
  for(i in assetvec) {
    corvec[i] <- cor(prets[, i], portrets[, "ec"])
  }

  return(corvec)

}  ########  END FUNCTION portfolio_cor #########


