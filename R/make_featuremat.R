####################################################################################
# FILE make_featuremat.R
#
# TODO:  Validate and test make_featuremat with complex features
#
####################################################################################
#
# FUNCTION make_featuremat
#
#' Generate a predictor xts matrix to use with a Machine Learning model
#'
#' This function assembles an xts matrix from a list of pre-calculated features.
#' The xts matrix is made up of a properly lagged first column named y (the target
#' to predict), and a number of additional columns specifying the feature vector.
#'
#' It is assumed that multiple xts matrices have been computed a priori and stored
#' in the feature_list, and each of these matrices hold the values of a given predictor.
#' A predictor (feature) may or may not be derived from the targeted asset.
#' For example, y may represent the 3 month momentum of price of a stock (possibly
#' lagged), while interest rates may be a feature extracted from the feature_list.
#'
#' Function make_features is normally called a priori to generate the list of features
#' specified in feature_list.
#'
#' In addition, the function can do a negative lag (lead) the predictor variable if
#' desired.
#'
#'
#'
#' @param feature_list  A list containing multiple xts matrices, each corresponding to a
#'                      feature. Each xts matrix may contain the feature for a universe of
#'                      assets, but only the column containing the string specified
#'                      by `symbol` as a subset of the column name will be extracted (see symbol
#'                      below). The FIRST feature in the list should be the target
#'                      variable (y) if this
#'                      is used to train a machine learning model via supervised learning.
#'
#' @param symbol        The name of the symbol in the form of a character vector of length 1.
#'                      This is used to extract the relevant column from each feature matrices
#'                      in the feature_list. Note that a column will be extracted even if the
#'                      symbol name has addition text prepended or appended.  For example, if
#'                      symbol = "SPY", a column named "xxx_SPY_mom4" will be extracted.
#'
#' @param Nlags         The number of periods (days), either positive or negative, by which
#'                      each specified feature will be lagged.  The specified features is
#'                      an internally built vector comprising target (if specified), followed
#'                      by featuresub (if specified).  If featuresub is not specified, then
#'                      the entire feature_list is used.  Nlags is padded with zeroes on the
#'                      right if it doesn't specify the lag for all features.  Default is 1,
#'                      meaning that the target is lagged by 1 day.
#'
#' @param featuresub    Specifies the names of features to subset from the feature_list
#'                      if only a subset of the features should be used to build
#'                      the feature matrix.  Default is NA, which means all features in the
#'                      feature_list will be used.  Note that the name of the target
#'                      variable column (y) should be explicitly specified as the FIRST item
#'                      in this list if the resulting matrix is used by a machine learning
#'                      model.
#'
#' @param verbose       Logical. Prints diagnostics on console. Useful to ensure the data
#'                      is set up as expected. Default is FALSE.
#'
#' @return  Returns an xts matrix with the following columns:
#' \describe{
#'   \item{\preformatted{y:}}{
#'      Assuming a target
#'      The target variable appropriately named as y. This column is moved forward in
#'      time by Nlags periods to reflect that y is in the future.  NA padding at the tail
#'      is done as appropriate so its obvious that the more recent vectors don't have y values
#'      and should therefore be predicted by a trained model.
#'
#'      }
#'   \item{\preformatted{predictor columns (kept in order):}}{
#'      The extracted predictor columns, kept in the order specified in prednames.
#'
#'      }
#' }
#'
#' @seealso \code{\link{make_features}}
#'
#' @export
#-------------------------------------------------------------------------------------
make_featuremat <- function(feature_list, symbol, Nlags = 1, featuresub = NA,
                            verbose = FALSE) {

  # Determine which features we want to extract from feature_list
  if(is.na(featuresub[1])) prednames <- c(names(feature_list)) else
    prednames <- c(featuresub)

  # Pad Nlags with 0 if shorter than length(prednames)
  if(length(Nlags) < length(prednames)) {
    Nlags <- c(Nlags, rep(0, length(prednames) - length(Nlags)))
  } else {
    Nlags <- Nlags[1:length(prednames)]
  }

  names(Nlags) <- prednames
  if(verbose) {
    sprint(">>> Lags are set as follows in feature matrix:")
    print(Nlags)
  }


  # Extract all predictors from the features list
  pred <- xts()
  for(i in prednames) {
    x    <- feature_list[[i]]

    # match consecutive characters in colnames of x to symbol
    cn       <- colnames(x)
    cn_which <- cn[stringr::str_detect(cn, symbol)]

    # extract it and merge to pred
    x   <- x[, cn_which, drop = FALSE, check.names = FALSE]
    colnames(x) <- i
    pred <- cbind(pred, x)

  }

  # make sure the column names are correct with special characters
  colnames(pred) <- prednames

  # Clean up the index to have date only, no GMT times
  pred    <- xts(pred, order.by = as.Date(index(pred)))
  predmat <- pred[, prednames]

  # Lag each feature as specified by Nlags
  for(i in 1:length(Nlags)) {
    predmat[, i] <- xts::lag.xts(predmat[, i], k = Nlags[i])
  }


  return(predmat)
}

#####################################################################################

#------------------------------------------------------------------------------------
#  This is the old version that uses the environment to get its data, rather
#  than a list.
#' @export
#-------------------------------------------------------------------------------------
make_predmatold <- function(y, prednames, symbol, Nlags = c(1, rep(0, length(prednames))),
                            envir = as.environment(-1)) {

  # Check for errors
  #sprint("Length Nlags: %s,  Length prednames: %s", length(Nlags), length(prednames))
  if(length(Nlags) != length(prednames) + 1)
    stop("make_predmat:  length(Nlags) must equal length(prednames) + 1")

  # Extract all predictors from the parent environment (prednames)
  pred <- xts()
  for(i in prednames) {
    x   <- get(i, inherits = TRUE, envir = envir)

    # match consecutive characters in colnames of x to symbol
    cn       <- colnames(x)
    cn_which <- cn[str_detect(cn, symbol)]

    # extract it and merge to pred
    x   <- x[, cn_which, drop = FALSE]
    colnames(x) <- i
    pred <- merge(pred, x)
  }

  # Clean up the index to have date only, no GMT times
  pred <- xts(pred, order.by = as.Date(index(pred)))


  #------------------------------------------------------------
  # Build the matrix and lag each column by their
  # respective Nlags value.
  #------------------------------------------------------------
  y           <- y[, symbol]
  colnames(y) <- "y"
  predmat  <- merge(y, pred[, prednames])

  for(i in 1:length(Nlags)) {
    predmat[, i] <- xts::lag.xts(predmat[, i], k = Nlags[i])
  }
  #predmat  <- merge(xts::lag.xts(y, k = -Nlag), pred[, prednames])
  #  predmat <- predmat[complete.cases(predmat), ]

  return(predmat)
}

