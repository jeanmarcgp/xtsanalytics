####################################################################################
# FILE rankscore.R
#
####################################################################################
#
#' Calculate the rankings and regularization scores of a universe of assets
#'
#' Calculate the pairwise rankings and regularization scores between all
#' assets in a universe vs. a portfolio of assets that is rebalanced daily.
#' The regularization formula considers both daily correlation and daily volatility.
#'
#' The portfolio (portassets) must be made up of assets present in the universe
#' otherwise an error will result. The assets in the universe are the
#' named columns in the daily returns matrix (rets).
#'
#' For each asset present in the universe AND not in the portfolio,
#' a regularization score is calculated according to the formula:
#'
#' \deqn{reguscore =  1 + \rho(cor + \epsilon)^\eta + \sigma * relvol}
#'
#' Similarly, a momentum score (momscore) is calculated from the asset
#' momentum divided by the above reguscore value:
#'
#' \deqn{momscore = asset momentum / reguscore}
#'
#'
#' A named vector is returned with the above regularization score for each
#' asset not in the portfolio compared with the portfolio.  The portfolio may be
#' built of equal weights or a set of predefined weights, as specified
#' by maxwtsvec. The quantity cor corresponds to the correlation of the asset vs.
#' the portfolio over the timeframe specified by the index of rets, the returns
#' xts matrix provided.  Similarly, relvol corresponds to the relative volatility
#' between the asset and the portfolio as calculated using the standard deviation
#' of returns over the timeframe specified by the rets matrix.
#'
#' \describe{
#' \item{rho \eqn{(\rho)}}{Regularization parameter applied to correlation term}
#' \item{epsilon \eqn{(\epsilon)}}{Small optional offset added to correlation value}
#' \item{eta \eqn{\eta}}{Norm exponent applied to correlation term}
#' \item{sigma \eqn{\sigma}}{Regulation parameter applied to relative volatility}
#' }

#'
#'
#' @param rets        An xts matrix of asset returns that include all assets
#'                    in the universe which must also include the assets in the
#'                    portfolio. The regularization score and its terms are
#'                    calculated over all returns provided by this matrix rets.
#'
#' @param portassets  A character vector containing the names of assets in
#'                    the portfolio. Must contain at least one name.
#'
#' @param maxwtsvec   Either an ordered or named numeric vector containing the
#'                    relative weight of each asset in the universe or a single
#'                    number. The sum of all weights does not need to equal
#'                    one (in fact, it would normally exceed one), as
#'                    this is used for relative weightings.  If a single number
#'                    (for example maxwtsvec is set to 1) then it results in
#'                    an equal weights portfolio using portassets.
#'
#' @param momvec      A named numeric vector containing annualized momentum values
#'                    for each asset in the universe.  If set to NA, then a momentum
#'                    value is computed inside the function using the equivalent
#'                    equity curve from the rets matrix argument (last row / first row).
#'
#' @param rho         The regularization parameter to control the amount of
#'                    correlation penalty between the asset and the portfolio
#'                    during regularization. Default is zero which means that
#'                    correlation is not penalized during regularization.
#'
#' @param abscor      Logical. Should the absolute value of correlations be used
#'                    (negative correlations are considered as positive) or not.
#'                    Default is FALSE (no absolute values).  Setting this to TRUE
#'                    may be helpful when using inverse ETFs since these will be
#'                    negatively correlated AND also exhibit inverse performance.
#'                    NOTE:  When FALSE, one is added to epsilon to offset the
#'                    correlation values so it can vary from 0 to 2 instead of
#'                    -1 to +1.  This ensures that the regularized correlation term
#'                    always results in a positive term.
#'
#' @param sigma       The regularization parameter to control the amount of
#'                    relative volatility penalty taken during regularization.
#'                    The asset relative volatility is the asset volatility
#'                    divided by the portfolio volatility, where volatility is
#'                    defined to be the daily standard deviation over the
#'                    timeframe defined by the rets matrix. Default value
#'                    is zero which means that volatility is not penalized.
#'
#' @param epsilon     A small positive offset added to the correlation value
#'                    of the asset against the portfolio before regularization
#'                    is applied.  Default is zero.
#'
#' @param eta         The exponent value for the correlation term.  This is
#'                    equivalent to the norm in a standard regularization problem.
#'                    Default is 1.
#'
#' @return   Returns a list of the following named vector. Each vector contains
#'           a value for an asset present in the universe and not in the portfolio.
#'
#' \describe{
#'   \item{\preformatted{$regurank}}{
#'      A named vector containing the regurank for each asset.  The regurank
#'      is the relative ranking of the regularization score (reguscore) for each asset.
#'      }
#'   \item{\preformatted{$momrank}}{
#'      A named vector containing the momrank for each asset. The momrank is the
#'      relative ranking of the momscore (see below).
#'      }
#'   \item{\preformatted{$reguscore}}{
#'      A named vector of the regularization score for each asset. This is the result
#'      of the regularization formula described in the Details section.
#'      }
#'   \item{\preformatted{$momscore}}{
#'      A named vector of the regularized momentum score for each asset.  This quantity
#'      is the result of multiplying momvec by reguscore.
#'      }
#' }
#' @export
#----------------------------------------------------------------------------------
rankscore <- function(rets, portassets = colnames(rets)[1], maxwtsvec = 1,
                      momvec = NA, rho = 0, abscor = FALSE, sigma = 0,
                      epsilon = 0, eta = 1) {

  # ########  For code testing  #############
  # library(xtsanalytics)
  # rets  = ROC(xts_data, type = "discrete")["2007-05-01/2008-05-01", ]
  # portassets = c("QQQ") #, "EWN", "SPY")
  #
  # rho     = 0 #1
  # abscor  = FALSE
  # sigma   = 0 #1
  # epsilon = 0
  # eta     = 1
  # maxwtsvec = c(0.2, 0.3, 0.4, 0.35, 0.5, 0.6, 0.25, 0.1)
  # momvec   = NA
  #
  # ################################

  # Must have a portfolio of at least one asset!
  if(length(portassets) < 1) stop("portassets must contain at least one asset (CASH?)")

  N     <- nrow(rets)
  rets  <- rets[complete.cases(rets), ]
  if(N != nrow(rets)) {
    sprint("WARNING(rankscore): %s rows removed from rets due to NAs.", N - nrow(rets))
  }

  allassets <- colnames(rets)
  Uassets   <- length(allassets)   # Num. assets in Universe

  # Asset names to regularize (Universe not in portfolio)
  assetvec  <- allassets[!allassets %in% portassets]
  if(length(assetvec) == 0)
    stop("rankscore:  Universe must include at least one asset NOT in portfolio")

  Nassets   <- length(assetvec)    # Num. assets to regularize

  maxwtsvec <- recycle_better(maxwtsvec, allassets, default = 0)

  #--------------------------------------------------------------
  # Compute portfolio equity curve and volatility
  #--------------------------------------------------------------
  portwts     <- maxwtsvec[portassets]    # portfolio weights
  portwtsmat  <- emptyxts(cnames = portassets, rowfill = portwts,
                          order.by = index(rets))
  portrets    <- rets[, portassets] * portwtsmat
  wtssum      <- rep(sum(portwts), length(portassets))[1]
  portrets$ec <- as.numeric(apply(portrets, 1, function(x) sum(x) / wtssum))

  #--------------------------------------------------------------
  # Compute annualized portfolio SD and relvol for each asset
  # If portassets is CASH only, then set relvol relative to
  # average of universe volatility.
  #--------------------------------------------------------------
  portvol     <- rep(sqrt(252) * sd(portrets$ec), Nassets)
  assetvol    <- sqrt(252) * apply(rets[, assetvec], 2, sd)
  relvol      <- assetvol / portvol
  if(length(portassets) == 1 && portassets[[1]] == "CASH")
    relvol <- assetvol / mean(assetvol)

  #sprint("relvol is:")
  #print(relvol)

  #--------------------------------------------------------------
  # Compute the asset momentum if not provided
  #--------------------------------------------------------------
  if(is.na(momvec[[1]])) {
    assetec     <- cumprod_na(1 + rets[, assetvec])
    momvec      <- as.numeric(last(assetec)) / as.numeric(first(assetec)) - 1
    names(momvec) <- assetvec
  } else {
    if(is.null(names(momvec))) {
      stop("momvec must be a named vector")
    } else {
      if(length(momvec) != ncol(rets))
        stop("momvec must be the same size as the universe")
    }
  }

  # Ensure momvec only contains values for assetvec, in proper order
  momvec <- momvec[assetvec]

  #--------------------------------------------------------------
  # Compute portfolio correlation to each asset
  #--------------------------------------------------------------
  corvec      <- portfolio_cor(rets = rets, portassets = portassets,
                               maxwtsvec = maxwtsvec)

  #--------------------------------------------------------------
  # Compute reguscore and momscore
  #--------------------------------------------------------------
  if(abscor) corvec <- abs(corvec) else
    epsilon <- epsilon + 1  # Add 1 to epsilon if neg. correl allowed

  epsilon    <- recycle(epsilon, Nassets)
  reguscore  <- 1 + rho * (corvec + epsilon)^eta + sigma * relvol
  momscore   <- momvec / reguscore

  #--------------------------------------------------------------
  # Now rank these scores to get rankscore and regurank
  #--------------------------------------------------------------
  momrank    <- Nassets - rank(momscore) + 1
  regurank   <- Nassets - rank(reguscore) + 1

  #--------------------------------------------------------------
  # Build the list to return
  #--------------------------------------------------------------
  retlist <- list(regurank    = regurank,
                  momrank     = momrank,
                  reguscore   = reguscore,
                  momscore    = momscore)

  return(retlist)

}  ########  END FUNCTION rankscore #########

