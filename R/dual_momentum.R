####################################################################################
# FILE dual_momentum.R
#
#
####################################################################################
# FUNCTION dual_momentum
#
#' Implements the dual momentum strategy as implied by Antonacci's paper.
#'
#' If no interest bearing cash instrument is provided, then it is possible
#' that all assets will have negative momentum.  In such case, the function will
#' go to cash and return no interest (daily returns = 0)
#'
#' @param prices     An xts of prices for all assets
#'
#' @param assets     A vector of the asset symbols to use as the module's
#'                   dual_momentum universe.  All symbols should correspond to
#'                   a column in the prices xts.  If an interest bearing cash
#'                   instrument is desired, then it should be included here also
#'                   and always produce a positive momentum via interest.
#'
#' @param relmom     Either a numeric or an xts containing the momentum
#'                   values for all assets used to compare between assets (relative).
#'                   If a numeric value, then it is assumed to be the momentum period
#'                   in days and a momentum matrix will be computed from the prices
#'                   matrix.
#'
#' @param absmom     Either a numeric or an xts containing the momentum
#'                   values for all assets used to apply the absolute momentum filter.
#'                   An asset must have positive momentum to be considered.
#'                   If a numeric value, then it is assumed to be the momentum period
#'                   in days and a momentum matrix will be computed from the prices
#'                   matrix.
#'
#' @export
#-----------------------------------------------------------------------------------
dual_momentum <- function(prices, assets, relmom, absmom) {

  #---------------------------------------------------------------------------------
  # Test to ensure all symbols in assets are in prices
  #---------------------------------------------------------------------------------
  if(!all(assets %in% colnames(prices)))
    stop("dual_momentum:  assets contains symbols not found in prices xts.")

  # subset prices to contain only relevant assets
  prices <- prices[, assets]

  #---------------------------------------------------------------------------------
  # Calculate relative momentum xts if a number is provided
  #---------------------------------------------------------------------------------
  if(length(relmom) == 1 && is.numeric(relmom)) {
    feat   <- paste0("mom", relmom)
    temp   <- make_features(prices, features = feat, smooth = NA, on = "days")
    relxts <- temp[[feat]]

  } else {
    relxts <- relmom
  }
  relxts <- na.trim(relxts, sides = "left")

  #---------------------------------------------------------------------------------
  # Calculate absolute momentum xts if a number is provided
  #---------------------------------------------------------------------------------
  if(length(absmom) == 1 && is.numeric(absmom)) {
    feat   <- paste0("mom", absmom)
    temp   <- make_features(prices, features = feat, smooth = NA, on = "days")
    absxts <- temp[[feat]]

  } else {
    absxts <- absmom
  }
  absxts <- na.trim(absxts, sides = "left")

  #---------------------------------------------------------------------------------
  # Align both mom xts to same start date, without NAs.  Align rets.
  #---------------------------------------------------------------------------------
  if(index(relxts[1, ]) > index(absxts[1, ]))  absxts <- absxts[index(relxts), ] else
    relxts <- relxts[index(absxts), ]

  indices <- index(relxts)
  rets    <- ROC(prices, type = "discrete")
  rets    <- rets[indices, ]


  #---------------------------------------------------------------------------------
  # Find the max relative momentum in each row, then get associated absmom
  #---------------------------------------------------------------------------------
  absxts$max_col       <- max.col(relxts)     # not lagged
  absxts$max_col_lag   <- Lag(absxts$max_col, 1)

  xcol                 <- which(colnames(absxts) == "max_col")
  temp                 <- apply(absxts, 1, function(x) x[x[xcol]])
  absxts$abs_sel       <- Lag(sign(as.numeric(temp)))

  #---------------------------------------------------------------------------------
  # Get the dual momentum rets
  #---------------------------------------------------------------------------------
  rets$max_col_lag  <- absxts$max_col_lag
  xcol              <- which(colnames(rets) == "max_col_lag")
  temp         <- apply(rets, 1, function(x) x[x[xcol]])
  rets$relmom  <- as.numeric(temp)


  rets$dualmom <- ifelse(absxts$abs_sel > 0, rets$relmom, 0)

  #---------------------------------------------------------------------------------
  # Relative and dual momentum equity curves
  #---------------------------------------------------------------------------------
  ec <- cumprod_na(1 + rets[, c('relmom', 'dualmom')])

  return(ec)

}


