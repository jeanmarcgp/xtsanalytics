
####################################################################################
# FILE stitch_statfolio.R
#
#
# FUNCTIONS TO DO:
# . need a function to subset wfodata, to chart weights for a period subset
# . need a low level function underneath subfolio_curve, to just compute
#   an equity curve from a set of fixed weight, given a rebalancing set of
#   dates.  This is to replace Returns.rebalancing.
# . Also, the random portfolio function could be written using the subfolio and
#   stitch_statfolio functions...  Basically, create some fake wfodata that
#   includes equal weights (or 100 random weights?), then stitch 1000 curves and
#   return the average curve.  This may be faster and likely closer to reality with
#   monthly rebalancing...
#     -> Instead, custom write it.  rebalance monthly, randomly pick N Assets equal
#        weights, and build 1000 curves.
#
#
####################################################################################
# FUNCTION stitch_statfolio
#
#' Create statistical equity curves by randomly stitching a statfolio
#'
#' This function assumes that wfodata has been created using function
#' wfo_statfolio.  This latter function runs a portfolio optimization multiple
#' times at each rebalance dates, creating a set of slightly different weights
#' at each WFO rebalance date. Function stitch_statfolio then randonly picks a
#' set of these weights at each WFO rebalance date to build N equity curves.  The
#' resulting equity curves therefore show slightly different paths to help
#' portray how sensitive a portfolio rebalance strategy may be with respect to
#' asset weights.
#'
#'
#' @param wfodata         A statfolio with class optimize.portfolio.rebalancing, with the
#'                        additional items related to statfolio within element
#'                        $opt_rebalancing.  A statfolio is created by calling function
#'                        wfo_statfolio.
#'
#' @param Ncurves         The number of equity curves created and returned.
#'
#' @param max_offset      The maximum trading delay (in days) from the ideal
#'                        rebalancing date.
#'
#' @param offset_type     The type of trading delay offset to perform, if any.  If
#'                        set to "none", then no trading delay are performed, meaning
#'                        that the rebalancing is done on the day specified in the wfodata
#'                        list, at the market's close.  If "random" is specified, then a random
#'                        trading delay offset is performed from 0 to max_offset days.
#'                        If "fixed" is specified, then rebalancing is performed exactly
#'                        max_offset days after the day specified in the wfodata list.
#'
#'
#' @return  Returns an xts matrix of equity curves.  Each equity curve in the matrix
#'          corresponds to a selected path from randomly choosing one set of weight
#'          at each rebalance date, and with the rebalancing possibly delayed as specified
#'          by arguments max_offset and offset_type.
#'
#' @seealso wfo_statfolio
#'
#' @export
#-----------------------------------------------------------------------------------
stitch_statfolio <- function(wfodata, Ncurves = 10, max_offset = 0,
                             offset_type = "none", norm_weights = TRUE) {

  # ######################################
  # # arguments override for development
  # library(xtsanalytics)
  # load("./ignore-results/wfodata.Rdata")
  # Ncurves      = 1
  # max_offset   = 2
  # offset_type  = "none"
  # norm_weights = TRUE
  # Ncurves      = 10
  # ######################################


  returns     <- wfodata$R                # returns for entire WFO timeframe
  folio       <- wfodata$opt_rebalancing  # List of each WFO optimization
  folionames  <- names(folio)             # vector of WFO dates
  Nwfo        <- length(folionames)       # Number of WFO dates

  if(Nwfo < 2) stop("stitch_statfolio:  Must have at least 2 subfolios to stich!")

  #-----------------------------------------------------------------
  # Loop through each WFO date and calculate portfolio returns
  # Ignore last WFO date since equity curve ends on that day!
  #-----------------------------------------------------------------
  prets  <- list()
  for(i in 1:(Nwfo - 1)) {
    subfolio       <- folio[[i]]
    subfolio_span  <- folionames[i:(i + 1)]
    portfolio_rets <- subfolio_curve(subfolio      = subfolio,
                                     returns       = returns,
                                     subfolio_span = subfolio_span,
                                     max_offset    = max_offset,
                                     norm_weights  = norm_weights)
    prets[[i]]     <- portfolio_rets

  }

  #-----------------------------------------------------------------
  # Stitch equity curves from prets list of returns matrices
  #-----------------------------------------------------------------
  Nsubcurves    <- ncol(prets[[1]])   # Number of subfolio equity curves
                                      # This includes the average curve

  stitch_names <- c("Average_Weights", paste0("Stitch_", 1:Ncurves))
  ec_start     <- index(first(prets[[1]]))
  ec_end       <- index(last(prets[[length(prets)]]))
  wfo_rets     <- returns[paste0(ec_start, "/", ec_end), ]

  stitch_rets  <- emptyxts(cnames = stitch_names, nc = Ncurves + 1,
                           order.by = index(wfo_rets))
  #stitch_rets  <- stitch_rets[-1, ]  # Rebalance done at close of market
                                     # Must exclude day's return!
  rebal_dates  <- folionames
  for(i in 1:(Nwfo - 1)) {
    #----------------------------------------------------
    # i is the current subfolio block. Ignore last one
    # run_picks is a vector containing the columns randomly
    # picked from the subfolio block
    #----------------------------------------------------
    run_picks <- sample(Nsubcurves, Ncurves, replace = TRUE)

    # Make sure to compute the Average_weight curve in col #1
    run_picks <- c(1, run_picks)

    start_cut <- rebal_dates[i]
    end_cut   <- rebal_dates[i + 1]
    cut_dates <- paste0(start_cut, "/", end_cut)

    ec_cut    <- prets[[i]][cut_dates, ]
    ec_cut    <- ec_cut[-1, ]  # remove first row since rebalance is done EOD

    stitch_rets[index(ec_cut), ] <- ec_cut[, run_picks]
  }

  #----------------------------------------------------------------------
  # Compute equity curves and identify those representing key stats
  #----------------------------------------------------------------------
  stitch_ec <- cumprod_na(1 + stitch_rets)
  last_row  <- last(stitch_ec)
  min_i     <- which.min(last_row)
  max_i     <- which.max(last_row)
  colnames(stitch_ec)[min_i] <- "Worst"
  colnames(stitch_ec)[max_i] <- "Best"

  # Bring all key columns up front
  upfront   <- c("Average_Weights", "Best", "Worst")
  cnames    <- colnames(stitch_ec)
  new_order <- c(upfront, cnames[!(cnames %in% upfront)])

  stitch_ec <- stitch_ec[, new_order]

  #----------------------------------------------------------------------
  # Add assets equity curves if desired
  #----------------------------------------------------------------------
  # This should be calculated earlier, and use neworder to select
  # whether to include in return data.  Add argument include_assets = TRUE

  return(stitch_ec)

}   ##########  END stitch_statfolio FUNCTION  ##########


#-------------------------------------------------------------------------
# HELPER FUNCTION  - NOT EXPORTED
#
# Compute all daily equity curve within a folio set of dates
# Returns an xts of daily equity curves in the form of
# returns of the following:
#  . Portfolio average (Portfolio)
#  . Equity curves corresponding to each set of weights in $allweights,
#    assuming subfolio$allweights is not NULL.
#-------------------------------------------------------------------------
subfolio_curve <- function(subfolio, returns, subfolio_span,
                           max_offset, norm_weights = TRUE) {

  # # arguments override for development
  # library(xtsanalytics)
  # load("./ignore-results/wfodata.Rdata")
  # subfolio      <- wfodata$opt_rebalancing[[1]]
  # subfolio_span <- names(wfodata$opt_rebalancing)[1:2]
  # max_offset    <- 0
  # norm_weights  <- TRUE
  # returns       <- wfodata$R
  # ###############################

  span_i        <- returns[subfolio_span, , which.i = TRUE]
  span_i[1]     <- span_i[1]
  span_i[2]     <- span_i[2] + max_offset      # offset for delay in next rebalance
  if(span_i[2] > nrow(returns))
    span_i[2] <- nrow(returns)                 # ensure we don't exceed last date!
  span_vec      <- span_i[1]:span_i[2]         # subset of returns needed

  rets          <- returns[span_vec, ]   # subset of returns of interest
  rets[1, ]     <- 0                     # rebalance done AFTER close, so ignore rets that day!
  ecassets      <- cumprod_na(1 + rets)  # assets equity curve


  #--------------------------------------------------------------
  # Compute equity curve for subfolio average weights
  #--------------------------------------------------------------
  avgwts        <- subfolio$weights
  if(norm_weights) avgwts <- avgwts / sum(avgwts)   # Normalize sum(weights) to one

  Necurves      <- nrow(subfolio$allweights)
  ecnames       <- c("Average", paste0("Run_", 1:Necurves))
  ec            <- emptyxts(cnames = ecnames, nc = Necurves + 1, order.by = index(ecassets))

  asset_wts     <- emptyxts(cnames = colnames(ecassets), nc = ncol(ecassets), order.by = index(ecassets))

  asset_wts[]   <- (t(avgwts * t(ecassets)))
  ec$Average <- as.numeric(apply(asset_wts, 1, sum))

  #--------------------------------------------------------------
  # Compute equity curves for every weight rows in allweights
  #--------------------------------------------------------------
  for(i in 1:Necurves) {
    wts   <- subfolio$allweights[i, ]
    #print(wts)
    asset_wts[]      <- (t(wts * t(ecassets)))
    ec[, (1 + i)]    <- as.numeric(apply(asset_wts, 1, sum))
  }


  #--------------------------------------------------------------
  # Convert equity curves xts to returns xts
  #--------------------------------------------------------------
  allrets      <- TTR::ROC(ec, type = "discrete")
  allrets[1, ] <- 0   # No returns on day 1 - rebalanced at EOD!

  # Maybe best instead subset by allrets[-1, ]

  return(allrets)
}


