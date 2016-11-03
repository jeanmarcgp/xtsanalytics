####################################################################################
# FILE wfo_grow_forest.R
#
#
#
####################################################################################
# FUNCTION wfo_grow_forest
#
#' Performs walk-forward optimization using the random forest algorithm
#'
#' Builds a random forest model at each optimization point in time for
#' a given set of parameters.  This function is single-threaded and builds
#' a single model at each point in time.  Use function wfo_grow_jungle to
#' take advantage of multiple CPU cores.
#'
#'
#' In order to ensure no date is
#'                    skipped, both start date and end date must align on a wfo_span
#'                    date.  In addition, the start date for a subsequent job should be
#'                    identical to the end date of the previous job.  This would correspond
#'                    to a prediction date in the previous job at <end date>, whereas it
#'                    would correspond to a model training date for the subsequent job
#'                    at <start date>.  The default is NA which means no subsetting is
#'                    performed.
#'
#' @param featuremat  The xts matrix previously generated using function `make_featuremat`.
#'                    It should contain the target y in column 1, and all the predictors
#'                    (features) in the other columns. It is a daily xts matrix.
#'
#' @param modelwindow Size in days for the rolling window use to build each model.
#'                    Each model is built at the dates specified by wfo_span and uses
#'                    a data training window of this size which includes the wfo_span date.
#'
#' @param wfo_span    Specifies the points in time when new models are optimized for
#'                    walk-forward optimization. The points in time can either be
#'                    given as a vector of date strings, or it can be automatically
#'                    generated using the endpoints function.  To use the endpoints
#'                    function, wfo_span must be a string of length one comprising
#'                    one of the following values: 'days', 'weeks', 'months' and 'quarters'.
#'                    This value is then passed on as argument "on" to function endpoints.
#'                    If 'days' is specified, then function endpoints is not used
#'                    since the data already has daily granularity. Default is "months".
#'
#' @param ylag        Set to number of days that y looks ahead in the feature matrix.
#'                    In order to avoid lookahead bias, the data set used to train the
#'                    model must use data that is earlier than this the model date minus
#'                    ylag.  Always set ylag to the number of lookahead days.  Default
#'                    is 21.
#'
#' @param SPwindow    The rolling window size in days used to calculate the SP_score. See
#'                    below for details on the SPscore.
#'
#' @param PQwindow    The rolling window size in days used to calculate the PQ_score.
#'                    See below for details on how the PQ_score is calculated.
#'
#' @param jobname     If specified, then the results are stored in a list with a single
#'                    item in it, with the jobname as its name. This is normally used
#'                    when multiple parallel calls are made to wfo_grow_forest as it
#'                    provides a simple method to combine all results.  See Value
#'                    section below for details.
#'
#' @param mtry        Parameter mtry passed on to randomForest function.  This is the
#'                    number of features randomly selected for each tree.  Defautl is 2.
#'
#' @param ntree       Parameter ntree passed on to randomForest function.  This is the
#'                    number of trees generated for each model.  Default is 1000.
#'
#' @param importance  Parameter importance passed on to randomForest function. This is a
#'                    logical specifying whether to output the variable importance.
#'                    Default is TRUE.
#'
#' @param earliest    The earliest possible wfo date desired.  This is useful if
#'                    the featuremat goes much further back in time than needed.
#'
#' @param na.action   Parameter passed on to randomForest to determine what to do with
#'                    NAs.  Default is na.omit.
#'
#' @param ...         Additional parameters passed on to randomForest function.
#'
#'
#' @return If jobname is specified, then the function wraps all list items below into
#'         a single element list, where the elemenat is named by argument jobname.
#'         Otherwise, it skips the wrapping and simple returns a list with the following items:
#'
#' \describe{
#'   \item{\preformatted{$pred}}{
#'      A daily xts matrix containing the prediction information. The index represent the prediction
#'      date. The matrix has the following columns:
#'      \itemize{
#'         \item
#'         \strong{y        } The target value against which each model are being trained, as
#'                            specified by column y in argument `featuremat`. For example,
#'                            this could be a future return for a certain period, properly lagged
#'                            in time.
#'         \item
#'         \strong{yhat    } The value predicted by the model trained using the most recent
#'                            date up to but not including the current period.
#'         \item
#'         \strong{samesign } This is a logical series comparing the signs between y and yhat
#'                            to see if they are the same. It is useful to convert the
#'                            random forest regression values (yhat) to a classification model.
#'
#'         \item
#'         \strong{SP_score } The Stability of Prediction score expressed as the number of daily
#'                            prediction sign changes over a rolling window of size `SPwindow`.  A
#'                            stable prediction should have a small SPscore such as < 2 or 3 for a
#'                            reasonably sized window. An unstable model will tend to oscillate and
#'                            should be viewed as indecisive.
#'         \item
#'         \strong{PQ_score}  The Prediction Quality score expressed as the number of daily prediction
#'                            that were of the same sign over a rolling window of size `PQwindow`.
#'                            The PQ_score is expressed as the ratio of accurate predictions over
#'                            the total number of predictions (PQwindow).  Therefore if all predictions
#'                            were correct over a given PQwindow, then PQ_score = 1.  A PQ_score
#'                            that is meaningfully below 1 should be viewed as bad because it means
#'                            the most recent models don't predict very well.
#'
#'
#'         }
#'
#'      }
#'
#'    \item{\preformatted{$wfo_points}}{
#'      \strong{A vector of time indices} containing the wfo optimization dates.  In other words,
#'      these are the dates at which a new model was optimized.
#'      }
#'
#'
#' }
#'
#' @export
wfo_grow_forest <- function(featuremat, modelwindow = 252, wfo_span = "months",
                            ylag = 21, SPwindow = 63, jobname = NA, verbose = FALSE,
                            mtry = 2, ntree = 1000, importance = TRUE, earliest = NA,
                            na.action = na.omit, ...) {


  #--------------------------------------------------------
  # Remove leading NAs in featuremat.
  #--------------------------------------------------------
  featuremat  <- zoo::na.trim(featuremat, sides = "left", is.na = "any")

  wfo_data   <- wfo_timeframe(featuremat = featuremat, modelwindow = modelwindow,
                              wfo_span = wfo_span, ylag = ylag, earliest = earliest,
                              verbose = verbose)

  wfo_points <- wfo_data$wfo_points
  Nlast      <- wfo_data$Nlast


  #---------------------------------------------------------
  # Ensure featuremat columns have valid names before
  # calling random forest function in loop.
  #---------------------------------------------------------
  colnames(featuremat) <- make.names(colnames(featuremat), unique = TRUE)

  #---------------------------------------------------------
  # Set up the results xts matrix then loop
  # on each modeling date
  #---------------------------------------------------------
  res        <- featuremat[(wfo_points[1] + 1):Nlast, 1, drop = FALSE]
  res$yhat   <- NA
  Ndates     <- length(wfo_points)
  for(i in 1:Ndates) {

    #----------------------------------------------------------
    # If last date in loop, then predict on that date using
    # the previous model, otherwise train model on date.
    #----------------------------------------------------------
    if(i == Ndates) {
      # Use previous model to predict on last date, so interval = Nlast
      interval <- Nlast

    } else {
      # Otherwise, train a model, then predict forward.  Set
      # interval between two consecutive wfo_points
      interval <- (wfo_points[i]+1):(wfo_points[i + 1])

      #--------------------------------------------------------
      # subset the data for training
      # Make sure we look back by ylag days to train the model.
      #--------------------------------------------------------
      train_start <- wfo_points[i] - modelwindow + 1 - ylag
      train_end   <- wfo_points[i] - ylag
      datatrain   <- featuremat[train_start:train_end, ]

      # Train random forest model
      train_dates  <- paste0(index(datatrain[1,]), "/", index(datatrain[nrow(datatrain), ]))
      if(verbose) sprint("Model training date: %s, training set: %s, No. features: %s, ylag: %s",
                         index(featuremat[wfo_points[i],]), train_dates, ncol(datatrain) - 1, ylag)

      rf  <- randomForest(y ~ ., data = datatrain, mtry = mtry, ntree = ntree,
                          importance = importance, na.action = na.action)
    }




    #------------------------------------------------
    # Predict next interval using most recent model
    #------------------------------------------------
    interval_dates <- paste0(index(featuremat[interval[1], ]), "/",
                             index(featuremat[interval[length(interval)], ]))
    if(verbose) sprint("Predicting %s using model trained on data ending %s\n",
                       interval_dates, index(datatrain[nrow(datatrain), ]))
    datapred <- featuremat[interval, -1]    # remove y
    yhat     <- predict(rf, datapred)
    res$yhat[interval - wfo_points[1]] <- yhat

  }  #####  END LOOP  #####

  #---------------------------------------------------------
  # Build the results list to return
  #---------------------------------------------------------
  res2  <- list(pred = res,
                wfo_dates = index(featuremat[wfo_points, ]))


  #---------------------------------------------------------
  # if jobname is specified, then wrap the results list in
  # a single element list named by variable jobname.
  #---------------------------------------------------------
  if(is.na(jobname)) results <- res2 else {
    results <- list(res2)
    names(results) <- jobname
  }


  return(results)

}


