#
####################################################################################
# FILE interpolate_returns.R
#
#
####################################################################################
# FUNCTION interpolate_returns
#
#' Interpolate returns in a time series containing NAs.
#'
#' An xts matrix of returns (1 or more columns is assumed).  The matrix
#' is first converted to an equity curve so intermediate prices can be
#' interpolated.
#'
#' @param rets   The xts matrix of returns, which contains some NAs to be
#'               interpolated.
#' @param value  Specifies whether the return value is an xts of returns ("returns") or
#'               a normalized equity curve "prices").  Default is "prices".
#'
#' @return Either an xts of returns or an xts of the equity curve, depending
#'         on the value argument.
#'
#' @export
#-----------------------------------------------------------------------------------
interpolate_returns <- function(rets, value = c("prices", "returns")) {

  # Replace any NAs in first row with zeros, so we can build an equity curve.
  rets[1, ] <- ifelse(is.na(rets[1, ]), 0, rets[1, ])

  ec <- cumprod_na(1 + rets)

  # fill flat ec sections with NAs, then use na.approx to interpolate
  for(i in 1:ncol(ec)) {
    ec[, i] <- ifelse(is.na(rets[, i]), NA, ec[, i])
    ec[, i] <- na.approx(ec[, i], rule = 2)
  }

  if(value[1] == "prices") retval <- ec else
    retval <- ROC(ec, type = "discrete")


  return(retval)

}
