
####################################################################################
# FILE rollcor.R
#
####################################################################################
#
#' Rolling correlation of one or multiple features vs. a target variable
#'
#' Over a sliding window, calculate the rolling correlation of an xts
#' matrix of features vs. a target variable.
#'
#'
#' @param features  The xts matrix containing the features.
#'
#' @param target    An xts matrix with identical indices as the features matrix.
#'                  If the target is a prediction further out in time, then
#'                  the time relationship between features and target should
#'                  be properly lagged prior to calling this function.
#'
#' @param width     The rolling window width used to calculate the rolling
#'                  correlation between the features and the target.
#'
#' @return  Returns an xts matrix of the same width as the feature matrix
#'          with the rolling correlation values.
#'
#' @export
#-------------------------------------------------------------------------------------
prediction_stability <- function(features, target, width = 63) {

  # ################  For code testing  ################
  # library(xtsanalytics)
  # data         = xts_gspc["2005/2016", ]
  # data         = xts_gspc["1980/1999", ]
  # featname     = c("mom126", "sd126")
  # targetname   = "sd21"
  # width        = 63
  # targetlag    = 21
  #
  # features     = make_features(data, features = featname)
  # features     = do.call(cbind, features)
  # colnames(features) <- featname
  # target       = make_features(data, features = targetname)[[1]]
  # colnames(target) <- targetname
  #
  # features     = lag(features, targetlag)
  #
  # ############

  nc <- ncol(features)

  ycnames <- paste0(colnames(features), "_", colnames(target))
  ycor    <- emptyxts(cnames = ycnames, order.by = index(features))

  for(i in 1:nc) {
    combo     <- xtsbind(features[, i], target)
    ycor[, i] <- rollapply(combo, width = width, by.column = FALSE, fill = NA,
                           align = "right", FUN = function(z)
                             cor(as.numeric(z[, 1]), as.numeric(z[, 2])))

  }

 # xtsplot(ycor, norm = FALSE, ylab = "Correlation",
  #        main = "Correlation of feature vs. target")


  return(ycor)


}   ########  END Function prediction_stability  ########
