#------------------------------------------------------------------------------------
#  FUNCTION mget_symbols
#
#' Get multiple symbols and store in a single xts matrix
#'
#' This function uses quantmod getSymbols to download multiple symbols
#' from a data source.  It then extracts one set of values (column) from
#' the OHLC data for each symbol and stores it into a wide xts matrix for
#' easy reference. Proper data columns are selected by invoking the appropriate
#' function from OHLC.Transformations in quantmod.
#'
#' For example, if value = "Cl", then quantmod::Cl() function is invoked to
#' extract the Close column for each symbol.
#'
#' If a symbol does not have enough history, leading NAs
#' are introduced for that symbol.  If the startdate provided is
#' earlier than all dates when symbol data is available, then theses dates
#' are omitted.
#'
#' @param  symbols    A vector of symbol names to download (character vector).
#'
#' @param  from       The start date from which the data is downloaded.
#'
#' @param  OHLC       Specifies the value to be extracted from the OHLC data.
#'                    The default is "Ad" for adjusted close.  Must be
#'                    one of c("Ad", "Cl", "Op", "Hi", "Lo")
#'
#' @param  src        Source where to find the data.  If src = "database", then
#'                    the symbol is searched in the local database with path
#'                    as specified in argument filepath.  Otherwise, this is
#'                    passed to quantmod::getSymbols function.
#'
#' @param filepath    The file path where to find the local database.  Default
#'                    is: "../DATABASE/data". This is used only if src = "database".
#'                    In that case, a file name will automatically be created
#'                    consisting of the symbol string concatenated with ".csv".
#'
#' @param  locf       Last observation carried forward. When TRUE, the resulting
#'                    prices matrix is subjected to function na.locf to carry
#'                    forward the last observation and therefore eliminate any
#'                    NAs in the price matrix.  This is relevant when some prices
#'                    are available at different days than others and NAs are introduced
#'                    within the matrix (beyond the beginning of the time series).
#'                    If these NAs are not eliminated, then computing returns from
#'                    the matrix will produce some NA returns and therefore some
#'                    information will be lost, creating an incomplete equity curve.
#'                    Subjecting the matrix to na.locf addresses this issue.
#'                    Default is TRUE.
#'
#' @param  ...        Additional arguments passed to quantmod::getSymbols
#'
#' @return Returns an xts matrix starting at startdate or later if no symbol
#'         data is available at the earlier dates.
#'
#' @examples
#' x <- mget_symbols(c('SPY', 'QQQ', 'EWC', 'GLD'), startdate="1999-03-01")
#' head(x, 15)
#' y <- mget_symbols(c('SPY', 'QQQ', 'EWC', 'GLD'), startdate="1999-03-01", OHLC="Cl")
#' head(y, 15)
#' @export
#------------------------------------------------------------------------------------
mget_symbols <- function(symbols, from="1999-01-01", OHLC="Ad", src = "yahoo",
                         filepath = "../DATABASE/data", locf = TRUE, ...) {


  #if(exists("stocks_data", where = sys.frame(which = 0))) sprint("stocks_data exists already") else
  #  sprint("stocks_data does not yet exist")

  stocks_data <- new.env(parent = parent.frame())
  if(!all(OHLC %in% c('Ad', 'Cl', 'Op', 'Hi', 'Lo', 'Vo')))
    stop('OHLC must be any of "Ad", "Cl", "Op", "Hi", "Lo" or "Vo".')

  symbols    <- unique(symbols)
  nc         <- length(symbols)

  if(src == "database") sprint("Loading data from local database...") else
    sprint("Getting data from %s...", src)

  dnload_sym <- NULL
  err_sym    <- NULL
  sprint("Downloading symbols: %s\n", stringr::str_c(symbols, collapse = " "))
  for(i in symbols) {
    if(src == "database") {
      #------------------------------------------------------
      # Load the data from the local database
      #------------------------------------------------------
      filename <- paste0(filepath, "/", i, ".csv")
      cat(paste0(" ", i))
      x <- as.xts(read.zoo(file = filename, #format = "%Y-%m-%d",
                           header = TRUE, drop = FALSE, sep = ","))
      colnames(x) <- paste0(colnames(x), ".Adjusted")
      assign(i, x, envir = stocks_data)
      dnload_sym <- c(dnload_sym, i)

    } else {
      #------------------------------------------------------
      # Get the symbol data from the web
      #------------------------------------------------------
      tryCatch({
        ####  using quantmod::getSymbols generate a warning whereas not
        ####  using quantmod:: does not.  Don't know why.
        quantmod::getSymbols(Symbols = i, from = from, env = stocks_data,
                             src = src, ... = ...)
        #quantmod::getSymbols(Symbols = i, from = from, env = stocks_data,
        #                     src = src)
        sprint("Successfully downloaded symbol: %s", i)
        dnload_sym <- c(dnload_sym, i)
      },
      error = function(e) {
        sprint("  >>>> ERROR DOWNLOADING SYMBOL::%s:: <<<<", i)
        #err_sym  <- c(err_sym, i)
      }
      )
    }  ####  END ELSE condition  ####

  }  ### FOR loop ###

  # If any symbol is an index, must remove the leading caret ^ to access it
  dnload_sym2 <- gsub("^", "", dnload_sym, fixed = TRUE)

  #-----------------------------------------------------------------------
  # First list layer is OHLC description
  # Second layer is the symbol data
  # Example:  z$Ad$GSPC to get to adjusted data
  #-----------------------------------------------------------------------
  z <- list()
  for(j in OHLC) {
    symdata <- lapply(mget(dnload_sym2, envir = stocks_data), function(x)
    { do.call(j, list(x))})
    z[[j]] <- symdata
  }


  #-----------------------------------------------------------------------
  # If more than one OHLC function was requested, then data is a list
  # of matrices, one matrix for each OHLC function.
  # Otherwise, data is an xts (simplify by removing the list layer)
  #-----------------------------------------------------------------------
  if(length(OHLC) > 1) {
    data <- list()
    for(k in OHLC) {
      data[[k]] <- do.call(xts::cbind.xts, z[[k]])
      colnames(data[[k]]) <- dnload_sym2
    }
  } else {
    data <- do.call(xts::cbind.xts, z[[1]])
    colnames(data) <- dnload_sym2
  }


  sprint("\n")
  sprint("DOWNLOAD SUMMARY:")
  sprint('=================')
  sprint('Symbols successfully downloaded: %s ',
         stringr::str_c(dnload_sym2, collapse = " "))

  err_sym <- symbols[!(symbols %in% dnload_sym)]
  sprint('Symbols NOT downloaded: %s\n', stringr::str_c(err_sym, collapse = " "))


  if(locf) {
    sprint("Applying na.locf to the matrix...")
    data <- na.locf(data)
  }
  return(data)


}

