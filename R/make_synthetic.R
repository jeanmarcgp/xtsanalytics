#
#========================================================================================
#
# make_synthetic.R
# ----------------
#
#' Creates a synthetic time series by adding white Gaussian noise to its returns
#'
#' This function works as follows:  The price returns are computed.  Then
#' a rolling standard deviation of these returns is calculated for each
#' price series in the matrix.  An SMA is then applied to stabilize the SD values.
#' The resulting varying SD matrix is then used to compute the additive noise matrix,
#' where the varying SD matrix is used to generated normal noise of SD = that matrix.
#' Last, this normal noise matrix is multiplied by the noise gain and added to the
#' returns, to create a synthetic series with Additive White Gaussian Noise (AWGN).
#'
#' The objective is to add noise of the same amplitude as the current standard deviation
#' of the underlying price series, so as to always inject a similar percentage of noise to
#' the series.
#'
#' This function can generate multiple independent synthetic series.
#'
#'
#' @param prices    An xts series of prices.  May contain multiple columns.
#'                  The synthetic series returned will have the same structure, so
#'                  AWGN (Additive White Gaussian Noise) will be added to each
#'                  series on a per item basis.
#'
#' @param noisegain This is the amount of fixed gain, expressed as a multiplier
#'                  to the standard deviation, to add to each value.
#'
#' @param sdwindow  The time window to compute the rolling standard deviation from
#'                  the prices.
#'
#' @param smawindow The time window to compute the rolling SMA from the rolling
#'                  standard deviation matrix.  This essentially smooths out the sd
#'                  matrix.
#'
#' @param N         The number of independent synthetic series to create.
#'
#' @param by_symbol If FALSE, then the list returned is of length N, and the underlying
#'                  xts contain one instance of each synthesized symbol.
#'
#'                  If TRUE, then the list returns is organized by symbols and the
#'                  underlying xts contain all synthesized instances of the said symbol.
#'
#'
#' @return  If the number of columns provided in the prices matrix is 1, then
#'          an xts is returned with N independent time series.  Argument by_symbol is
#'          ignored.
#'
#'          If the number of colums provided in the prices matrix is > 1, then
#'          a LIST of xts is returned. If by_symbol = TRUE, then each element in the
#'          list will correspond to a symbol, and each xts under that symbol will contain
#'          N synthetic series.  If by_symbol = FALSE, then each list element contains an
#'          instance of synthetic series, and each xts contains one instance of synthetic
#'          prices for each price column provided.
#'
#'
#' @export
#========================================================================================
make_synthetic <- function(prices, noisegain = 0.5, sdwindow = 21, smawindow = 10,
                           N = 1, by_symbol = TRUE) {

  # ##### For testing #####
  # library(xtsanalytics)
  # prices     = xtsbind(xts_data[, "SPY"], xts_gspc[,])
  # #prices     = xts_data[, "SPY"]
  # prices     = prices[complete.cases(prices), ]
  # noisegain  = 0.5
  # sdwindow   = 21
  # smawindow  = 10
  # N          = 3
  # by_symbol  = T
  # #######################


  #-------------------------------------------------------------
  # Set up the features
  #-------------------------------------------------------------
  sdfeature  <- paste0("sd", sdwindow)
  smafeature <- paste0("sma", smawindow)

  prices     <- prices[complete.cases(prices), ]
  rets       <- ROC(prices, type = "discrete")
  sdrolling  <- make_features(prices, features = sdfeature, by_symbol = FALSE)[[1]]
  sdsma      <- make_features(sdrolling, features = smafeature, by_symbol = FALSE)[[1]]
  sdsma      <- sdsma[complete.cases(sdsma), ]
  rets       <- rets[index(sdsma), ]

  #-------------------------------------------------------------
  # Create the noise matrix, the add it to the rets matrix
  # Main loop: repeat N times.
  #-------------------------------------------------------------
  namat    <- rets
  namat[]  <- NA
  synthlist <- vector("list", N)
  names(synthlist) <- paste0("S", 1:N)
  for(nseries in 1:N) {
    noise <- namat

    for(i in 1:nrow(noise)) {
      for(j in 1:ncol(noise)) {
        noise[i, j] <- rnorm(1, 0, sd = noisegain * sdsma[i, j])
      }
    }
    synthrets   <- rets + noise
    synthprices <- cumprod_na(1 + synthrets)

    # Store synthprices in a list
    synthlist[[nseries]] <- synthprices
  }

  sprint("N = %s", N)
  if(N == 1) synthlist <- synthlist[[1]] else {
    # # Name all synthesized series as S<x>_<parent>
    # for(ilist in 1:N) {
    #   cnames <- do.call(paste0, list(names(synthlist)[ilist], "_", colnames(synthlist[[ilist]])))
    #   colnames(synthlist[[ilist]]) <- cnames
    # }
    #### This works but blows up flip_xtslist because the xts colnames are now different


    if(by_symbol) synthlist <- flip_xtslist(synthlist)

  }



  return(synthlist)

}
