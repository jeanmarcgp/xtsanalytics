####################################################################################
# FILE wfo_grow_forest.R
#
#
# NEXT STEPS:  Test wfo_grow_forest with a basic testthat, and
# make sure it has basic tests for the wfo_subsets, while
# ensuring there is no lookahead bias.
# Also, add the adjusted (no lookahead) equity curve code to the function
# and return list.
####################################################################################
# FUNCTION wfo_grow_forest
#
#' Performs walk-forward optimization using the random forest algorithm
#'
#' This function builds a random forest model at each optimization point in time for
#' a given set of parameters.  It is single-threaded and builds a single model at each
#' point in time.  To run multiple models at each point in time using multiple CPUs,
#' use function wfo_grow_jungle.
#'
#'
#' @param featuremat  The xts matrix previously generated using function `make_featuremat`.
#'                    It should contain the target y in column 1, and all the predictors
#'                    (features) in the other columns. It is a daily xts matrix.
#'
#' @param modelwindow Size in days for the rolling modeling window.  Each model is built
#'                    at the dates specified by wfo_span and uses a data window data of
#'                    this size that includes the wfo_span date.
#'
#' @param wfo_span    Specifies the points in time when new models are optimized for
#'                    walk-forward optimization. This uses the endpoints function to
#'                    extract the dates from the featuremat index.  Valid values include
#'                    'days', 'weeks', 'months' and 'quarters'.  If 'days' is specified,
#'                    then function endpoints is not used since the data already has
#'                    daily granularity. Default is "months".
#'
#' @param wfo_subset  Use to specify how to run a over a subset of featuremat dates.
#'                    Normally used by \strong{wfo_grow_jungle} to spawn multiple
#'                    parallel jobs, each covering a subset of dates. The dates should
#'                    specified as an xts timeframe as follows:
#'                    \strong{<start date>/<end date>}.  In order to ensure no date is
#'                    skipped, both start date and end date must align on a wfo_span
#'                    date.  In addition, the start date for a subsequent job should be
#'                    identical to the end date of the previous job.  This would correspond
#'                    to a prediction date in the previous job at <end date>, whereas it
#'                    would correspond to a model training date for the subsequent job
#'                    at <start date>.  The default is NA which means no subsetting is
#'                    performed.
#'
#' @param SPwindow    The rolling window size in days used to calculate the SP_score. See
#'                    below for details on the SPscore.
#'
#' @param PQwindow    The folling windo size in days used to calculate the PQ_score.
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
#'         \strong{ypred    } The value predicted by the model trained using the most recent
#'                            date up to but not including the current period.
#'         \item
#'         \strong{samesign } This is a logical series comparing the signs between y and ypred
#'                            to see if they are the same. It is useful to convert the
#'                            random forest regression values (ypred) to a classification model.
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
#'    \item{\preformatted{$wfo_dates}}{
#'      \strong{A vector of time indices} containing the wfo optimization dates.  In other words,
#'      these are the dates at which a new model was optimized.
#'      }
#'
#'
#' }
#'
#' @export
wfo_grow_forest2 <- function(featuremat, modelwindow = 252, wfo_span = "months",
                            wfo_subset = NA, SPwindow = 63, jobname = NA,
                            mtry = 2, ntree = 1000, importance = TRUE,
                            na.action = na.omit, ...) {


  #--------------------------------------------------------
  # Remove leading NAs, then extract the wfo_span dates
  #--------------------------------------------------------
  featuremat <- featuremat[complete.cases(featuremat), ]


  # Extract the wfo_span date: remove 0 and most recent date.
  # since we can't develop a model on the last date (target y = NA)
  # --Keep most recent date and in loop, set a flag such that this
  # last date is a prediction only (skip the new model)
  wfo_dates  <- endpoints(featuremat, on = wfo_span)[-1]


  # Subset wfo_dates further to ensure data exists on first training date
  start_win  <- wfo_dates - modelwindow + 1
  wfo_dates  <- wfo_dates[start_win > 0]

  # If wfo_subset is specified, then further subset the wfo_span dates
  # and truncate featuremat appropriately
  if(!is.na(wfo_subset)) {
    sub_dates      <- unlist(stringr:str_split(wfo_subset, "/"))

    # Check for bad wfo_subset arguments and throw an error if misaligned.
    if(!(as.Date(sub_dates[1]) %in% index(featuremat[wfo_dates, ]) &&
         as.Date(sub_dates[2]) %in% index(featuremat[wfo_dates, ]) )) {
      stop("wfo_grow_forest:  Arguments wfo_subset dates don't align with wfo_span dates.")
    }



    featuremat     <- featuremat[1:featuremat[sub_dates[2], , which.i = TRUE]]
    wfo_span_dates <- index(featuremat[wfo_subset, ])
    wfo_span       <- featuremat[wfo_span_dates, , which.i = TRUE]

    sprint("DIAGNOSTIC: wfo_span_dates and wfo_span are:")
    print(wfo_span_dates)
    print(wfo_span)
  }



  N          <- nrow(featuremat)

  ### HERE subset wfo_dates further using wfo_subset timeframe
  ### Also reset N to be the last date on featuremat by truncating
  ### featuremat

  # Ensure the featuremat column names are valid before calling
  # main Random Forest loop.
  colnames(featuremat) <- make.names(colnames(featuremat), unique = TRUE)

  #---------------------------------------------------------
  # Set up the results xts matrix then loop
  # on each modeling date
  #---------------------------------------------------------
  res        <- featuremat[(wfo_dates[1] + 1):Nlast, 1, drop = FALSE]
  res$ypred  <- NA
  Ndates     <- length(wfo_dates)
  for(i in 1:Ndates) {

    # Extract the date interval between the model dates
    # If it's the last date, then this is a flag:
    # -- no model built, just predict on that last date.
    #if(i == Ndates) interval <- (wfo_dates[i] + 1):N else
    #  interval <- (wfo_dates[i]+1):(wfo_dates[i + 1])


    # Diagnostic to see which models predict which date intervals
    #intervalstr <- str_c(paste(interval), collapse = ", ")
    #sprint("model date = %s, interval = %s ", wfo_dates[i], intervalstr)

    if(i == Ndates) {
      # Use previous model to predict on last date, so interval = N
      interval <- N

    } else {
      # Otherwise, must build a model, then predict forward.  Set
      # interval between two consecutive wfo_dates
      interval <- (wfo_dates[i]+1):(wfo_dates[i + 1])

      # subset the data for training
      datatrain <- featuremat[(wfo_dates[i] - modelwindow + 1):wfo_dates[i], ]

      # Train random forest model
      sprint("Building model on: %s, Number of features: %s",
             index(featuremat[wfo_dates[i],]), ncol(datatrain) - 1)
      rf  <- randomForest(y ~ ., data = datatrain, mtry = mtry, ntree = ntree,
                          importance = importance, na.action = na.action)
    }

    # Predict next interval with trained model
    datapred <- featuremat[interval, -1]    # remove y
    ypred    <- predict(rf, datapred)
    res$ypred[interval - wfo_dates[1]] <- ypred

  }

  #---------------------------------------------------------
  # Build the results list to return
  #---------------------------------------------------------
  res2  <- list(pred = res,
                wfo_dates = index(featuremat[wfo_dates, ]))


  #---------------------------------------------------------
  # if jobname is specified, then wrap the results list in
  # a one element list named by variable jobname.
  #---------------------------------------------------------
  if(is.na(jobname)) results <- res2 else {
    results <- list(res2)
    names(results) <- jobname
  }


  return(results)

}


