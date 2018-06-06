###################################################################################
#
#  FUNCTIONS IN THIS FILE:  xtsoverlay.R
#   .xtsoverlay
#   .xtsbind is now moved to file genpurpose.R
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


  # ##################
  # library(xtsanalytics)
  #
  # data            = xts_data["2007-01/2007-02", 1]
  # dates_i     = endpoints(data, on = "weeks")[-1]
  # dates_dates = index(data[dates_i, ])
  #
  # timezero        = dates_dates
  # offsets         = c(-5, 10)
  # norm            = TRUE
  #
  # ##################

  N  <- nrow(data)
  nc <- ncol(data)
  timezero <- as.Date(timezero)
  tzlen    <- length(timezero)
  cnames <- colnames(data)


  #--------------------------------------------------------------------------
  # If we have more timezero dates than data columns provided, then
  # create a matrix that recycles the data columns such that we end up
  # with ncol == length(timezero).  Each column will then be aligned
  # at the respective timezero
  #--------------------------------------------------------------------------
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

  #--------------------------------------------------------------------------
  # Loop over each column (representing one period each)
  #--------------------------------------------------------------------------
  for(i in 1:nc) {

    # Extract the column to analyze
    datacol  <- data[, i, drop = FALSE]

    # timezero date may fall on weekend, so get the next available date
    t0_actual <- index(data[index(data) >= timezero[i],, drop=FALSE])[1]
    t0_i      <- data[as.Date(t0_actual), i, which.i=TRUE]

    # First and last row index in data
    tstart_i  <- t0_i + offsets[1]
    tend_i    <- t0_i + offsets[2]

    #-----------------------------------------------------------------------
    # skipzoo is used to treat incomplete series (stuff with NAs)
    # at the beginning or end of the series
    #-----------------------------------------------------------------------
    skipzoo <- FALSE
    if(tstart_i < 1) {
      sprint("WARNING: xtsoverlay: starting before available data indices.")
      skipzoo <- TRUE
    }

    if(tend_i   > N) {
      sprint("WARNING: xtsoverlay: ending beyond available data indices.")
      skipzoo <- TRUE

    }

    if(!skipzoo) {
      zoocol           <- as.matrix(zoo(as.numeric(datacol[tstart_i:tend_i, 1]),
                                        order.by = timeframe), ncol = 1)

    } else {
      # Adjust for starting before 1 or ending beyond N
      zoocol    <- as.matrix(zoo(NA, order.by = timeframe), ncol = 1)

      if(tstart_i < 1) {
        # Keep NA at front end, the rest with datacol data
        tadjust                 <- 2 - tstart_i
        Nzoo                    <- length(zoocol)
        zoocol[tadjust:Nzoo, 1] <- zoo(as.numeric(datacol[1:(Nzoo - tadjust + 1), 1]),
                                       order.by = timeframe[tadjust:Nzoo])
      }

      if(tend_i > N) {
        # Keep NAs at back end, the rest with datacol data
        icount <- N - tstart_i + 1    # number of datapoints to include

        zoocol[1:icount, 1]  <- zoo(as.numeric(datacol[tstart_i:N, 1]),
                                    order.by = timeframe[1:icount])
      }

    }  ###### END else statement  ######

    #----------------------------------------------------------------------
    # Name the column with proper timezero element
    #----------------------------------------------------------------------
    colnames(zoocol) <- paste0(colnames(datacol), "_", timezero[i])

    #----------------------------------------------------------------------
    # cbind zoocol to zoo_ret
    #----------------------------------------------------------------------
    if(is.null(zoo_ret)) zoo_ret <- zoocol else
      zoo_ret <- cbind(zoo_ret, zoocol)

  }  #####  END FOR LOOP  ####


  # print(zoo_ret)

  # Normalize at timezero if specified
  offset0 <- -offsets[1] + 1
  if(norm) {
    zoo_ret[]  <- apply(zoo_ret, 2, function(x) x / rep(x[offset0], length(x)))
  }

  zoo_ret <- as.zoo(zoo_ret)

  return(zoo_ret)
}



