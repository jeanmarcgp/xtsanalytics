###################################################################################
#
#  FUNCTIONS IN THIS FILE:  xtsoverlay.R
#   .xtsoverlay
#   .xtsbind
#
###################################################################################


#----------------------------------------------------------------------------------
#  FUNCTION xtsoverlay
#
#' Aligns time series to a common overlap point (time zero).
#'
#' This function aligns two or more time series to a common overlap point
#' denoted time zero.  Thus, multiple time series of unrelated dates can
#' be compared by overlapping and normalizing
#'
#'
#'
#'
#' @param data      The xts matrix data containing all time series to align.
#'                  Each column corresponds to one time series that will be
#'                  aligned based upon its associated time zero. Use function
#'                  xtsbind to bind multiple xts matrices with different time
#'                  indices.
#'
#' @param timezero  A vector of dates denoting the time zero for
#'                  each column in the data xts to align.  Its length must
#'                  equal the number of columns in the data xts.
#'
#' @param offsets   A numeric vector of length 2 containing the negative
#'                  and positive offsets with respect to timezero used to
#'                  extract the data from the xts matrix.
#'
#' @param norm      Logical.  Specifies whether the curves are normalized
#'                  at time zero.
#'
#' @return Returns a zoo matrix (Note:  NOT xts) containing all the time
#' series with numeric offsets instead of dates.
#'
#' @export
#----------------------------------------------------------------------------------
xtsoverlay<- function(data, timezero, offsets = c(-20, 100), norm=TRUE) {


  N  <- nrow(data)
  nc <- ncol(data)
  timezero <- as.Date(timezero)
  tzlen    <- length(timezero)
  cnames <- colnames(data)

  if(tzlen > nc) {
    ntimes  <- ceiling(tzlen / nc)
    colnums <- rep(1:nc, ntimes)[1:tzlen]
    cnames2 <- rep(cnames, ntimes)[1:tzlen]
    data    <- data[, colnums]
    colnames(data) <- cnames2
  }

  nc <- ncol(data)
  if(length(timezero) != nc)
    stop("xtsalign: timezero vector length must equal number of data columns.")

  timeframe <- offsets[1]:offsets[2]

  zoo_ret <- NULL

  # Loop over each column
  for(i in 1:nc) {
    # timezero date may fall on weekend, so get the next available date
    t0_actual <- index(data[index(data) >= timezero[i],, drop=FALSE])[1]
    t0_i      <- data[as.Date(t0_actual), i, which.i=TRUE]
    #sprint("t0_1 = %s", t0_i)
    tstart_i  <- t0_i + offsets[1]
    tend_i    <- t0_i + offsets[2]

    #sprint("tstart_i = %s", tstart_i)
    #sprint("tend_i = %s", tend_i)
    #sprint("nc = %s", nc)

    skipzoo <- FALSE
    if(tstart_i < 1) {
      sprint("WARNING: xtsalign: starting before available data indices.")
      skipzoo <- TRUE
    }

    if(tend_i   > N) {
      sprint("WARNING: xtsalign: ending beyond available data indices.")
      skipzoo <- TRUE
    }

    if(!skipzoo) {
      zoocol  <- zoo(as.numeric(data[tstart_i:tend_i, i]), order.by = timeframe)

      if(is.null(zoo_ret)) zoo_ret <- zoocol else
        zoo_ret <- zoo::cbind.zoo(zoo_ret, zoocol)
    }



  }

  # Name the columns: colnames with timezero stamp
  colnames(zoo_ret) <- paste0(colnames(data), "_", timezero)

  # Normalize at timezero if specified
  offset0 <- -offsets[1] + 1
  if(norm) {
      data <- data[complete.cases(data), , drop=FALSE]
      coredata(zoo_ret) <- apply(zoo_ret, 2, function(x) x / rep(x[offset0], length(x)))

  }

  return(zoo_ret)
}


#----------------------------------------------------------------------------------
#  FUNCTION xtsbind
#
#' Binds xts columns from two or more xts matrices.
#'
#' This functions is similar to cbind in that it binds xts matrices column-wise.
#' If the xts matrices do not have the same number of rows, it will appropriately
#' pad with NAs.  In addition, it binds on date (not time), so if the prices
#' are sampled at different times, then the times are ignored for proper binding.
#'
#' @param x      First xts matrix to bind.
#' @param y      Second xts matrix to bind.
#' @param ...    Additional xts matrices to bind.
#'
#' @return Returns a an xts matrix that cbinds all matrices provided as
#'         arguments, padded with NAs as needed.
#' @export
#----------------------------------------------------------------------------------
xtsbind <- function (x, y, ...) {
  xts::cbind.xts(x, y, ...)

}