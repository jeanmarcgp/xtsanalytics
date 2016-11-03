####################################################################################
# FILE subset_statfolio.R
#
####################################################################################
# FUNCTION subset_statfolio
#
#' Subset a statfolio using standard xts index formatting
#'
#' @param statfolio  The statfolio to subset
#'
#' @param tf         The timeframe specified in standard xts format.
#'
#' @return  A new statfolio that is a time subset of the original statfolio.  The
#'          daily returns xts matrix $R is not subsetted.  Only the $op_rebalancing
#'          portion is subsetted.
#'
#' @export
#-----------------------------------------------------------------------------------
subset_statfolio <- function(statfolio, tf = "all") {

  # #######################
  # ####  Devel code  #####
  # #######################
  # library(xtsanalytics)
  # load("./ignore-results/wfodata.Rdata")
  # statfolio = wfodata
  # tf        = "2011-10/2012-03"
  # ############################

  if(tf != "all") {
    rets <- statfolio$R
    wfonames  <- names(statfolio$opt_rebalancing)
    wfoxts    <- emptyxts(order.by = as.Date(wfonames))
    wfoselect <- as.character(index(wfoxts[tf]))
    newfolio  <- statfolio
    newfolio$opt_rebalancing <- statfolio$opt_rebalancing[wfoselect]

  } else {
    newfolio  <- statfolio
  }

  return(newfolio)

}  ##########  END FUNCTION subset_statfolio  ##########

#--------------------------------------------------------------------------
#' Extract the SD weights of a statfolio for further processing
#'
#' @param statfolio  The statfolio from which to extract the SDweights
#'
#' @param tf         The statfolio timeframe of interest, in xts index format
#'
#' @return Returns an xts containing the weights SD of each asset at each
#'         WFO date.
#'
#' @export
#---------------------------------------------------------------------------
extract_SDweights <- function(statfolio, tf = "all") {

  # #######################
  # ####  Devel code  #####
  # #######################
  # library(xtsanalytics)
  # load("./ignore-results/wfodata.Rdata")
  # statfolio = wfodata
  # tf        = "2011-10/2012-03"
  # ############################

  newfolio    <- subset_statfolio(statfolio, tf = tf)
  wfonames    <- names(newfolio$opt_rebalancing)
  assetnames  <- colnames(newfolio$R)

  SDweights   <- emptyxts(cnames = assetnames, order.by = as.Date(wfonames))
  N           <- nrow(SDweights)

  for(i in 1:N) {
    SDweights[i, ] <- newfolio$opt_rebalancing[[i]]$SD_weights
  }

  return(SDweights)

}
