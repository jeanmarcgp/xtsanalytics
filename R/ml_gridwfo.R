
####################################################################################
# FILE ml_gridwfo.R
#
####################################################################################
#
#' Walk-Forward Optimization using grid search at each optimization date
#'
#' Performs a grid search of multiple machine learning models at each
#' walk-forward optimization date.
#'
#' This approach differs from normal WFO as follows.  At each date,
#' a number of ML models are trained, each with its own set of paramters.
#' The parameters may include different feature sets, evaluation threshold
#' values or even ML model hyper-parameters.  Each of these models are then
#' validated using a validation set, and a score is given to each model.
#' The model with the highest score wins, and its parameter set are taken
#' as the optimal set for building the final model.
#'
#' The final model is then trained using a partial combination of the training
#' set combined with the validation set.  Only part of the training set is used
#' because the date range is assumed to be constant for grid models and the final
#' model.  This final model is then used to predict the next period from the
#' WFO date onwards.
#'
#' @param datamat       The data xts matrix or dataframe containing the target
#'                      variable (y) and the features.
#'
#' @param features      A vector containing the names or numeric numbers of the
#'                      datamat columns containing the features.  This is
#'                      provided to subset datamat if other ID columns are also
#'                      included in datamat (which would be ignored). Default is
#'                      NA, which means that all columns except ycol and datecol
#'                      are taken as valid features.
#'
#' @param ycol          The name or numeric number of the column in datamat
#'                      containing the target variable.
#'
#' @param datecol       The name or numeric number of the column in datamat
#'                      containing the date at which a prediction is made.
#'                      This is used only if class(datamat) is a data.frame,
#'                      and therefore multiple predictions may happen on the
#'                      same date.
#'
#' @param on            The periods used to determine the WFO dates.  This
#'                      uses xts dates either from datamat (if an xts) or
#'                      from the inner merge of datamat with dateseries if
#'                      datamat is a dataframe.  Function xts::endpoints()
#'                      is used to extract the dates, an the value of on is
#'                      passes directly to this function.  Valid values include
#'                      'weeks', 'months', 'quarters' and 'years'.
#'
#' @param trainperiod   The size of the training period (a positive integer)
#'                      used to train the grid models and the final model.  This
#'                      number corresponds to the number of WFO periods used
#'                      for training.  For example, if the WFO periods are in quarters
#'                      and trainperiod = 8, then the training period will be 2 years.
#'
#' @param validperiod   The size of the validation period used to validate the
#'                      grid models.  This number must be less than trainperiod.
#'
#' @param dateseries    An xts matrix containing the dates (as its index) used to
#'                      build the returned xts matrix.  See Value returned for
#'                      details.
#'
#' @param gridFUN       The name of the function used by the grid search algorithm
#'                      to build and validate the ML models.  The following arguments
#'                      are passed to gridFUN:  the training data, the validation data
#'                      and a dateseries if class(datamat0 = "data.frame". gridFUN
#'                      should return an equity curve which is then evaluated by the
#'                      scoringFUN (see below).
#'
#' @param gridPAR       A dataframe containing the parameters used by
#'                      each grid model. Each row in the dataframe corresponds to
#'                      to the parameters used by one grid model. One column in this
#'                      dataframe should contain the feature set. Internally, an
#'                      exhaustive grid search will be performed to train and validate
#'                      each model using gridFUN and scoringFUN.
#'
#' @param gridfeatcol   The name of the column in gridPAR that contains the feature
#'                      set used by the grid models.  NOTE:  these features must
#'                      be a subset of the features available in datamat, otherwise
#'                      an error will result.
#'
#' @param scoringFUN    The name of the scoring function to evaluate the
#'                      performance of each grid model.  The model's equity
#'                      curve is passed to the function (an xts matrix), and it
#'                      should return a numeric score.  The grid model with the
#'                      highest score is considered the best model.  The following
#'                      functions are implemented internally:  "CAGR", "MAR" and
#'                      "Sharpe".
#'
#' @return  Returns an xts matrix with indices comprising the inner merge of
#'          dateseries merged with the datecol dates in datamat, in the case where
#'          datamat is a dataframe.  If datamat is an xts matrix, then dateseries
#'          is ignored and the indices of datamat are used.
#'
#'          The xts matrix includes the following 3 columns:
#'
#' \itemize{
#'   \item
#'     \strong{Equity_Curve } The Equity Curve resulting from the gridwfo
#'                            search and optimization.
#'
#'     \strong{Allstocks    } If class(datamat) == "data.frame", then this column
#'                            corresponds to the number of assets available to
#'                            trade on the current date (the index of the returned
#'                            xts matrix). If class(datamat) == "xts", then this
#'                            column is excluded.
#'
#'     \strong{Ntrades      } If class(datamat) == "data.frame", then this column
#'                            contains the number of trades performed on the current
#'                            date.  If class(datamat) == "xts", then this column
#'                            is excluded.
#'
#' }
#'
#' @export
#-------------------------------------------------------------------------------------
ml_gridwfo <- function(datamat, features, ycol = 1, datecol = NA, on = "months",
                       trainperiod = 8, validperiod = 2, dateseries = NULL,
                       gridFUN = "trade_overnight", gridPAR = NULL,
                       gridfeatcol = "Features", scoringFUN = "CAGR" ) {


  ######### Code for testing ONLY   #########
  library(xtsanalytics)
  features = c("Perc52WkHiRank", "PQMaxNDayRetRank", "PQMinNDayRetRank")
  datamat  = Earnings[, c(1, 2, 13, which(colnames(Earnings) %in% features))]

  ycol        = 1
  datecol     = 2
  on          = "months"
  trainperiod = 12
  validperiod = 6
  dateseries  = xts_data[, 1]

  ##############################################


  #------------------------------------------------------
  # Test for dataframe class, and extract WFO dates
  #------------------------------------------------------
  if(class(datamat) == "data.frame") dfclass <- TRUE else
    dfclass <- FALSE

  if(dfclass) {
    stopifnot(!is.null(dateseries))   # dateseries cannot be NULL

    firstdate <- as.Date(datamat[1, datecol])
    lastdate  <- as.Date(datamat[nrow(datamat), datecol])
    datadates <- unique(as.Date(datamat[, datecol]))
    dateindex <- index(dateseries[paste0(firstdate, "/", lastdate), ])

    #datadates <- c(datadates, as.Date("2001-01-01"))
    #----------------------------------------------------------------
    # Test that dateindex contains all datadates (plus inner dates)
    #----------------------------------------------------------------
    if(!all(datadates %in% dateindex)) {
      # Some dates in datadates are not in dateindex, so stop!
      bad_dates <- datadates[which(!(datadates %in% dateindex))]
      sprint("The following dates in datamat don't exist in dateseries:")
      print(bad_dates)
      stop("Stopping execution.  Check the dates!")
    }

    #---------------------------------------------------
    # Extract WFO dates
    #---------------------------------------------------
    wfoendpts <- endpoints(dateseries[dateindex, ], on = on)[-1]
    wfodates  <- dateindex[wfoendpts]
  } else {
    #---------------------------------------------------
    # Not dfclass, so stop for now (implement xts later)
    #---------------------------------------------------
    stop("class(datamat) != 'data.frame' so stopping execution.")

  }

  #------------------------------------------------------
  #
  #------------------------------------------------------

}
