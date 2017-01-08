
####################################################################################
# FILE riskparity_regularize.R
#
#
# TODO
#  .function portfolio_cor to compute the correlation of assets vs. EW portfolio
#  .
####################################################################################
#
#' Adjust max portfolio weights using risk parity regularization
#'
#' Screens assets from a universe and adjust the weights of each assets
#' according to a risk parity rule.  The assets selected are the
#' result of regularized momentum ranking based on raw asset momentum,
#' asset correlation against the portfolio and volatility.  In addition,
#' trading hysteresis is implemented to allow the use of a large universe
#' while limiting the amount of period to period trading.
#'
#'
#' @param prices      An xts matrix of asset prices.
#'
#' @param maxweights  A vector containing the maximum allowable weightfor each
#'                    asset as a fraction of the total portfolio. The vector
#'                    is recycled if not enough values are provided, or truncated
#'                    if it is too long.  Default is 0.5 for all assets.
#'
#' @param riskpar_feature  The volatility feature used to calculate the risk parity
#'                    maximum weight adjustment.  Risk parity in this context is
#'                    used to equalize risk for a given asset when compared
#'                    against its stated maximum weight.  For example, if an
#'                    asset has its maxweight set to 0.3, its risk parity
#'                    threshold set to 0.10 and its volatility at a given time
#'                    is 0.15.  Then its adjusted maxweight will be
#'                    0.3 * (0.10 / 0.15) = 0.20. Default is sd126.
#'
#' @param riskparity  The volatility threshold at which the asset weight
#'                    will be scaled back, according to the risk parity formula
#'                    min(1, riskparity / asset_volatility), where
#'                    asset_volatility is the most recent value of the
#'                    riskpar_feature feature, expressed as an annualized value.
#'
#' @param momfeature  The momentum feature applied to each asset for the
#'                    relative and absolute momentum screens.
#'
#' @param momthresh   The absolute momentum threshold below which an asset
#'                    is automatically excluded from ownership.  Expressed as
#'                    a fraction, not a percent.
#'
#' @param reguwindow   The rolling window (in days) used to calculate the
#'                    correlations at each WFO date.
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
#' @param wfo_span    Specifies the WFO span used to generate the wfo timeframe.
#'                    Can be the usual endpoints values including "days", "weeks",
#'                    "months"or "quarters".  If length(wfo_span) > 1, then it is
#'                    assumed that a vector of specific dates are provided.
#'
#' @param wfo_offset  Offset in days from the wfo_span date used to adjust the
#'                    final wfo dates.  May be positive or negative.
#'
#' @return Returns a list of the following items:
#'
#' \describe{
#'   \item{\preformatted{$maxweights}}{
#'      An xts matrix covering the wfo dates, and containing the maximum weights for
#'      each asset at the wfo dates.
#'      }
#'
#'   \item{\preformatted{$momscores}}{
#'      An xts matrix indexed at the wfo dates, and containing the adjusted momentum values
#'      regularized by the regularizing function.
#'      }
#'
#'   \item{\preformatted{$reguscore}}{
#'      An xts matrix indexed at the wfo dates containing the regularization scores for each
#'      asset i.e. not multiplied by the momentum as $momscore.
#'      }
#'
#'   \item{\preformatted{$wfodates}}{
#'      A character vector of all the wfo dates used.
#'      }
#'   }
#'
#' @export
#--------------------------------------------------------------------
riskparity_regularize <- function(prices, maxweights = 0.5, riskpar_feature = "sd126",
                                  riskparity = 0.12, momfeature = "mom252",
                                  momthresh = 0.05, reguwindow = 63, cashasset = NA,
                                  Nassets = ncol(prices), wfo_span = "months",
                                  wfo_offset = 0, sellrank = 3, dafaultweight = 0.35) {



  ##### Code for testing function only  ####
  library(xtsanalytics)
  prices     = xts_alldata[, c(1:18)]   # ensure no NAs in prices
  maxweights = c(0.5, 0.4, 0.3, 0.4)
  names(maxweights) <- c("BND", "SPY", "QQQ", "VTI")
  defaultweight = 0.35
  riskpar_feature = "sd63"
  riskparity  = 0.12
  momfeature = "mom84"
  momthresh  = 0.0
  reguwindow  = 63   # rename to rankwindow, for rankscore
  cashasset  = NA
  Nassets    = 5   # number of assets in portfolio, including CASH
  wfo_span = "months"
  wfo_offset = 0
  sellrank = 5
  rho      = 1
  sigma    = 1
  epsilon  = 0
  eta      = 1
  abscor   = FALSE
  daily_rate = 0.001 / 252

  ######  ISSUES
  # absmom window
  # relmom window
  # volatility window for riskparity
  # cor window for correlation (in rankscore)
  #
  # #########################################

  #-------------------------------------------------------------------
  # Remove NAs from prices.  If cashasset = NA, then create
  # default cashasset (CASH) in prices matrix.
  #-------------------------------------------------------------------
  prices     <- prices[complete.cases(prices), ]
  if(is.na(cashasset)) {
    prices$CASH <- cumprod_na(1 + rep(daily_rate, nrow(prices)))
    cashasset   <- "CASH"
  }
  cashname   <- colnames(prices[, cashasset])
  if(is.null(cashname)) stop("Must specify cashasset with a legal column number or name")

  #-------------------------------------------------------------------
  # Move cashasset to column 1. Compute rets and recycle maxweights
  # Force maxweights for CASH to 1.
  #-------------------------------------------------------------------
  prices     <- cbind(prices[, cashasset], prices[, colnames(prices) != cashname])
  cnames     <- colnames(prices)

  rets       <- ROC(prices, type = "discrete")
  rets       <- rets[complete.cases(rets), ]

  maxweights <- recycle_better(maxweights, colnames(prices), default = defaultweight)
  maxweights[cashname] <- 1

  #-------------------------------------------------------------------
  # Compute all features and normalize to annual quantities.
  #   .Calculate the momentum feature, normalized to annual
  #   .Calculate the volatility matrix, adjust daily to annualized
  #-------------------------------------------------------------------
  sprint("Computing feature matrices in riskparity_regularize...")
  momnum     <- as.numeric(stringr::str_extract(momfeature, "[[:digit:]]+"))
  momscaling <- 252 / momnum
  mommat     <- make_features(prices, features = momfeature)[[1]]
  mommat     <- (mommat + 1)^momscaling - 1
  mommat     <- na.locf(mommat, na.rm = TRUE)

  # Assume daily prices, sd features, so multiply sqrt(252) to annualize
  volmat     <- sqrt(252) * make_features(prices, features = riskpar_feature)[[1]]
  volmat     <- na.locf(volmat, na.rm = TRUE)

  #-------------------------------------------------------------------
  # Risk Parity:
  # Reduce each weight in maxwtsmat if volatility > riskparity
  #-------------------------------------------------------------------
  riskparity <- recycle(riskparity, ncol(volmat))
  maxwtsmat  <- emptyxts(cnames = colnames(volmat), rowfill = maxweights,
                         order.by = index(volmat))  # Start with maxweights matrix

  rpwts      <- maxwtsmat
  rpwts[]    <- t(apply(volmat, 1, function(x) {
    ifelse(x > riskparity, riskparity / x, 1)}))
  maxwtsmat  <- maxwtsmat * rpwts  # maxweights * risk parity weights

  #-------------------------------------------------------------------
  # Get the WFO dates and check date ranges for at least 2 dates
  # datemat[1, ] is the earliest possible WFO date.
  # Code into function wfo_getdates(mat, on = "months", offset = 0)
  #-------------------------------------------------------------------
  regudatemat <- rollapplyr(rets, width = reguwindow, FUN = sum) # for first date only
  datemat     <- xtsbind(maxwtsmat, mommat, regudatemat)
  datemat     <- datemat[complete.cases(datemat), ]  # Remove NAs for first date

  wfodates_i    <- endpoints(datemat, on = wfo_span)[-1] + wfo_offset
  wfodates_logi <- wfodates_i %in% 1:nrow(datemat)
  wfodates_i    <- wfodates_i[wfodates_logi]    # index
  wfodates      <- index(datemat[wfodates_i])   # actual dates
  if(length(wfodates) < 2)
    stop("riskparity_regularize: timeframe too short. Must have at least 2 WFO dates.")

  #-------------------------------------------------------------------
  # Align all matrices on WFO date except rets which needs
  # earlier daily history.  Create regumommat empty matrix.
  #-------------------------------------------------------------------
  maxwtsmat   <- maxwtsmat[wfodates, ]
  mommat      <- mommat[wfodates, ]
  volmat      <- volmat[wfodates, ]
  regumommat  <- emptyxts(cnames = colnames(maxwtsmat),
                         order.by = index(maxwtsmat))
  holdingsmat <- emptyxts(cnames = cnames, rowfill = 0,
                          order.by = index(maxwtsmat))

  #-------------------------------------------------------------------
  # Perform the absolute momentum screen, apply to maxwtsmat
  #-------------------------------------------------------------------
  absmom_logi  <- ifelse(mommat >= momthresh, 1, 0)
  maxwtsmat    <- maxwtsmat * absmom_logi

  #-------------------------------------------------------------------
  # Build the initial portfolio using cash asset
  #-------------------------------------------------------------------
  holdings <- cashname
  #holdings = c("CASH", "QQQ", "SPY", "XLB", "XLK", "XLI")

  #-------------------------------------------------------------------
  # WFO LOOP
  #  1. Sell the assets below sellrank (inner sell loop)
  #  2. Replace with top assets in #1 position (inner buy loop)
  #-------------------------------------------------------------------
  #wfodates <- wfodates[6]   # first 2 step loop

  sprint("=================================================")
  sprint("  Processing WFO dates in riskparity_regularize  ")
  sprint("=================================================")
  for(idate in 1:length(wfodates)) {
    #-----------------------------------------------------------------
    # Set up subrets, momvec and maxwtsvec at current WFO date
    # Screen assets universe for those passing absolute momentum only
    #-----------------------------------------------------------------
    ddate         <- wfodates[idate]
    cat(as.character(ddate), "")
    #sprint("===================================================")
    #sprint("   ddate is:  %s", ddate)
    #sprint("===================================================")
    maxwtsvec     <- maxwtsmat[ddate, ]
    maxwtsvec     <- as.vector(maxwtsvec)
    names(maxwtsvec) <- colnames(maxwtsmat)

    assetscreen   <- names(maxwtsvec[which(maxwtsvec > 0)])
    wfo_i         <- rets[ddate, , which.i = TRUE] # on WFO date EOD
    subrets       <- rets[(wfo_i - reguwindow + 1):wfo_i, ]

    momvec        <- mommat[ddate, ]
    momvec        <- as.vector(momvec)
    names(momvec) <- colnames(mommat)

    ### HERE, must have all assets in universe to compute sell ranks  ###
    #-----------------------------------------------------------------
    # STEP 1:  Sell assets of rank below sellrank if holdings
    # contain more than CASH.  Ranks must be compared to entire universe
    #-----------------------------------------------------------------
    if(length(holdings) > 1 || holdings[[1]] != "CASH") {

      # First Sales: Get rid of assets not passing the absmom screen
      holdings <- holdings[holdings %in% assetscreen]

      #sprint("Holdings after absolute momentum:")
      #print(holdings)
      #---------------------------------------------------------------
      # Inner Sell Loop:  Consider selling all holdings except CASH
      #---------------------------------------------------------------
      assets2sell <- NULL
      for(isell in holdings[!(holdings %in% "CASH")]) {
        portUT     <- holdings[!(holdings %in% isell)]
        assetranks <- rankscore(rets       = subrets,   portassets = portUT,
                                maxwtsvec  = maxwtsvec, momvec     = momvec,
                                rho        = rho,       abscor     = abscor,
                                sigma      = sigma,     epsilon    = epsilon,
                                eta = eta)

      # sprint("assetranks for isell = %s", isell)
        #print(assetranks)
        isellrank  <- assetranks[["momrank"]][isell]  # Get rank of isell asset
         #sprint("isellrank is %s: %s", names(isellrank), isellrank)
        if(isellrank >= sellrank) {
          # capture name of asset to sell
          assets2sell <- c(assets2sell, names(isellrank))
        }
      #  sprint("assets2sell at end of iteration:")
      #  print(assets2sell)
      }  ####  END Inner Sell Loop  ####

      #-----------------------------------------------------
      # Reduce holdings to assets to keep
      #-----------------------------------------------------
      holdings <- holdings[!(holdings %in% assets2sell)]

    }  ##### END if holdings > 1 #####

    # sprint("Sell loop completed. holdings are: ")
    #print(holdings)

    #-----------------------------------------------------------------
    # STEP 2: Fill up portfolio with best ranking assets
    #-----------------------------------------------------------------
    if(length(holdings) > Nassets)
      stop("Buy loop problem. More holdings than Nassets allowed!")

    # Start loop if portfolio not already full and there are assets to buy
    # sprint("assetscreen before buy loop:")
    #  print(assetscreen)
    assets2buy <- assetscreen[!(assetscreen %in% holdings)]
    loopmax    <- min(Nassets - length(holdings), length(assets2buy))

    if(loopmax >= 1) {
      #---------------------------------------------------------------
      # Inner Buy Loop:  Iterate with best assets available
      # Screen all matrices by assetscreen (positive momentum assets)
      #---------------------------------------------------------------
      momvec     <- momvec[assetscreen]
      maxwtsvec  <- maxwtsvec[assetscreen]
      subrets    <- subrets[, assetscreen]

      for(ibuy in 1:loopmax) {
        #sprint("ibuy iteration number: %s", ibuy)
        assetranks <- rankscore(rets       = subrets,   portassets = holdings,
                                maxwtsvec  = maxwtsvec, momvec     = momvec,
                                rho        = rho,       abscor     = abscor,
                                sigma      = sigma,     epsilon    = epsilon,
                                eta = eta)

        # Extract the top ranking asset name
        newasset <- names(assetranks$momrank[which(assetranks$momrank == 1)])
        # sprint("New asset to add: %s", newasset)

        # Add to holdings
        holdings <- c(holdings, newasset)
      }   #### END ibuy Inner Loop  ####

    }  ######  END if for buying assets  ######

    #----------------------------------------------------------------
    # Screen maxwtsmat with holdings for final maxwtsmat
    # Also, save holdings in holdingsmat
    #----------------------------------------------------------------
    notholdings   <- cnames[!(cnames %in% holdings)]
    if(length(notholdings) > 0) maxwtsmat[ddate, notholdings] <- 0

    holdingsmat[ddate, holdings] <- 1

    #----------------------------------------------------------------
    # Save holdings in holdingmat.
    # Maxwts * momentum as a simple diagnostic
    #----------------------------------------------------------------
    maxwtsmommat <- maxwtsmat * mommat

    #----------------------------------------------------------------
    # Build the regumommat here.  This uses CPU cycles
    # because we have to loop through all holdings, remove
    # the asset under test, call rankscore to obtain the regumom
    # for each asset.  SKIP FOR NOW.
    #----------------------------------------------------------------


  }  #######  END WFO Dates Loop  #######


  #-------------------------------------------------------------
  # Build the return list
  #-------------------------------------------------------------
  retlist <- list(maxwtsmat    = maxwtsmat,
                  mommat       = mommat,
                  holdingsmat  = holdingsmat,
                  maxwtsmommat = maxwtsmommat,
                  regumommat   = regumommat)  # NAs for now.

  return(retlist)


}  ########## END FUNCTION riskparity_regularize  ##########
