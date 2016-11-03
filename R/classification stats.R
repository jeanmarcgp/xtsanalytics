#
###################################################################################
#
# FILE classification stats.R
#
# Set of functions to produce machine learning / data mining statistics
# from the results of a classification or regression engine (including from
# the real number values of market timers).
#
# FUNCTIONS IN THIS FILE:
# -----------------------
#   . periods_in_class
#   . predictor_stats
#   . predictor_cor
#
###################################################################################
#
#----------------------------------------------------------------------------------
#  FUNCTION periods_in_class
#  -------------------------
#
#' Cumulatively counts the periods since the latest prediction
#'
#' This function computes the cumulative number of periods since the last prediction
#' (or transition).  For instance, a market timer is a form of binary classifier and
#' when applied to the timer series, this function will return the number of periods
#' since it last changed value.
#'
#' The return value is in the form of a column added to the xts matrix passed as the
#' data argument.  The added column contains the cumulative period count since the last
#' observed transition in the specified predictor column.
#'
#'
#'
#'
#' @param data     The xts matrix data containing the classifier's results column
#'                 (e.g. market timer).
#' @param col      The number or name of the column containing the classification information.
#' @param retcol   The name of the column containing the cumulative count results
#'                 (default is "cum_count").
#'
#' @return A column added to the xts matrix data argument.
#'
#' @export
#----------------------------------------------------------------------------------
periods_in_class <- function(data, col=1, retcol="cum_periods") {

  # Find the transition rows
  N <- nrow(data)
  vec <- ifelse(coredata(data[2:N, col]) != coredata(data[1:(N-1), col]), T, F)
  vec <- c(F, vec)
  no_transitions <- !vec

  # count the number of cumulative periods in the latest class
  # Use Rcpp to get speed

  Rcpp::cppFunction('NumericVector cumul_per_cpp(NumericVector no_transitions) {

    NumericVector vec = no_transitions;
    int n = no_transitions.size();
    int i = 0;

    vec[0] = 0;
    for(i = 1; i < n; i++ )
    {
      if (no_transitions[i] == 0 )
        vec[i] = 1;
      else
        vec[i] = vec[i-1] + 1;
    }

    return(vec);
  }
  ')
  vec <- cumul_per_cpp(as.numeric(no_transitions))


  # cbind cumulative periods to data and return data
  data$temp <- vec
  cn <- colnames(data)
  colnames(data) <- c(cn[1:length(cn)-1], retcol)
  return(data)

}  #####  END FUNCTION periods_in_class  #####

###################################################################################

#----------------------------------------------------------------------------------
#  FUNCTION predictor_stats
#  -------------------------
#
#' Predictive statistics of a classifier or a market timer
#'
#' This function returns a list of typical machine learning statistics for a binary
#' classifier (predictor) based on data from an xts matrix by comparing
#' the predictions vs. actuals.  The matrix must contain the asset equity curve and
#' a binary predictor class (1 = predict up market, 0 = predict down market).  This is
#' especially useful for analyzing the performance of market timers.
#'
#'
#'
#' @param data       The xts matrix data containing the equity curve of the asset or
#'                   portfolio, and the classifier's prediction column. e.g. market timer.
#' @param ec_col     The column name or column number of the equity curve under test.
#'                   Default = "ec".
#' @param timer_col  The column name or column number of the prediction (or timer).
#'                   Default is "GC".
#'
#'
#' @return  Returns a list of several objects.  Each item is defined as follows:
#' \describe{
#'   \item{\preformatted{$predictions:}}{
#'      An xts matrix containing the dates when predictions happen.  Columns are
#'      the equity curve, the prediction (from the timer), the actual equity curve
#'      results (indicating whether the market actually went up over the ensuing cycle,
#'      and four columns indicating whether the prediction ended up being a TP, FP,
#'      TN or FN (see predictions_stats for details).
#'      }
#'   \item{\preformatted{$N_predictions:}}{
#'      Number of predictions (transitions) observed, including the latest one
#'      where the actual is unknown.
#'      }
#'   \item{\preformatted{$predictor_stats:}}{
#'      The predictor statistics expressed as a vector of length 4 as c(TP, FP, FN, TN),
#'      where TP = True Positives, FP = False Positives, FN = False Negatives and
#'      TN = True Negatives.
#'      }
#'   \item{\preformatted{$confusion_matrix:}}{
#'      The transition statistics expressed as a standard confusion matrix.
#'      }
#'   \item{\preformatted{$confusion_ext:}}{
#'      The extended confusion matrix, with an added row summing the total predicted
#'      positives and negatives, and an added column summing the total actual positives
#'      and negatives.  The bottom right entry is the sum of all predictions, which must be
#'      one less than $N_predictions because this matrix includes only those predictions
#'      that have an associated actual value (thus, excludes the last prediction).
#'      }
#'   \item{\preformatted{$total:}}{
#'      The total number of predictions analyzed.  To be analyzed, a prediction must have an
#'      associated actual, so this excludes that latest prediction.
#'      }
#'   \item{\preformatted{$actual_pos:}}{
#'      The number of actual positives, as defined by the equity curve showing a positive
#'      return over the period between two predictions.
#'      }
#'   \item{\preformatted{$actual_neg:}}{
#'      The number of actual negatives, defined as the equity curve showing a negative
#'      return over the period between two predictions.
#'      }
#'   \item{\preformatted{$accuracy:}}{
#'      Answers the question:  \strong{"Overall, how often is the classifier correct?"}
#'      Defined as follows: \deqn{accuracy = (TP + TN) / total}
#'      Note that accuracy is not an appropriate measure for unbalanced classes.
#'      Precision and recall should be used instead.
#'      }
#'   \item{\preformatted{$misclass_rate:}}{
#'      Answers the question:  \strong{"Overall, how often is it wrong?"}
#'      The misclassification rate, aka \strong{"Error Rate"}, is defined as follows:
#'      \deqn{misclass_rate = 1 - accuracy = (FP + FN) / total}
#'      }
#'   \item{\preformatted{$TP_rate:}}{
#'      Answers the question:  \strong{"When actual is positive, how often does it
#'      predict positive?"}
#'      The True Positive rate, aka \strong{"Sensitivity"} or \strong{"Recall"}, is defined
#'      as follows:
#'      \deqn{TP_rate = TP / actual positives}
#'      }

#'   \item{\preformatted{$FP_rate:}}{
#'      Answers the question:  \strong{"When actual is negative, how often does it
#'      predict positive?"}
#'      The False Positive rate is defined as follows:
#'      \deqn{FP_rate = FP / actual negatives}
#'      }
#'   \item{\preformatted{$TN_rate:}}{
#'      Answers the question:  \strong{"When actual is negative, how often does it
#'      predict negative?"}
#'      The True Negative rate, aka as \strong{specificity} is defined as follows:
#'      \deqn{TN_rate = TN / actual negatives}
#'      }
#'   \item{\preformatted{$FN_rate:}}{
#'      Answers the question:  \strong{"When actual is positive, how often does it
#'      predict negative?"}
#'      The False Negative rate is defined as follows:
#'      \deqn{FN_rate = FN / actual positives}
#'      }
#'   \item{\preformatted{$specificity:}}{
#'      Answers the question:  \strong{"When actual is negative, how often does it
#'      predict negative?"}
#'      Specificity is equivalent to True Negative rate and is defined as:
#'      \deqn{specificity = TN / actual negatives = TN_rate}
#'      }
#'   \item{\preformatted{$precision:}}{
#'      Answers the question:  \strong{"What is the fraction of predicted positives
#'      are actually true positives?"}
#'      Stated differently, it is how often a positive prediction turns out to be correct.
#'      It is therefore a measure of confirmation.
#'      Precision is defined as:
#'      \deqn{precision = TP / predicted positives = TP / (TP + FP)}
#'      }
#'   \item{\preformatted{$recall:}}{
#'      Recall is the same as the True Positive rate. It is the fraction of the
#'      actual (true) positives that are detected by the classifier.
#'      It is therefore a measure of utility i.e. how much does the classifier find
#'      that is actually to be found.  See TP_rate above.
#'      }
#'   \item{\preformatted{$prevalence:}}{
#'      Answers the question: \strong{"How often does the positive condition happens in
#'      the data set?"}
#'      Prevalence is defined as:
#'      \deqn{prevalence = actual positives / total}
#'      }
#'   \item{\preformatted{$pos_pred_value:}}{
#'      The Positive Predictive Value is similar to precision except that it takes
#'      prevalence into account.  When the classes are balanced, PPV is identical to
#'      precision.
#'      \deqn{pos_pred_value = TP / (TP + FP)}
#'      CHECK THIS as this looks identical to precision!
#'      }
#'   \item{\preformatted{$null_error_rate:}}{
#'      The
#'      }
#'   \item{\preformatted{$F1_score:}}{
#'      The F1 score combines precision and recall into one figure. If either precision
#'      or recall is very small, then the F1 score will also be small.  The F1 score
#'      is defined as:
#'      \deqn{F1_score = 2 * precision * recall / (precision + recall)}
#'      }

#'
#' }
#'
#'
#' @export
#----------------------------------------------------------------------------------
predictor_stats <- function(data, ec_col="ec", timer_col="GC") {

  # Find and keep the transition rows only
  N <- nrow(data)
  vec <- ifelse(coredata(data[2:N, timer_col]) != coredata(data[1:(N-1), timer_col]),
                T, F)
  vec <- c(F, vec)
  #data$notransition <- !vec
  predictions <- data[vec, ]
  # Compute actual forward market direction (actual)
  N <- nrow(predictions)
  actual <- ifelse(coredata(predictions[2:N, ec_col])
                   > coredata(predictions[1:(N-1), ec_col]), 1, 0)
  predictions$actual <- c(actual, NA)

  # Add transition analysis columns:  TP, FP, TN, FN
  predictions$TP <- NA
  predictions$FP <- NA
  predictions$TN <- NA
  predictions$FN <- NA

  # Compute TP, FP, TN, FN
  predictions$TN <- ifelse(predictions[, timer_col] == 0 & predictions$actual == 0, 1, 0)
  predictions$TP <- ifelse(predictions[, timer_col] == 1 & predictions$actual == 1, 1, 0)
  predictions$FN <- ifelse(predictions[, timer_col] == 0 & predictions$actual == 1, 1, 0)
  predictions$FP <- ifelse(predictions[, timer_col] == 1 & predictions$actual == 0, 1, 0)

  # remove most recent row to compute stats
  tr <- predictions[1:(N-1), c('TP', 'FP', 'FN', 'TN')]

  # Compute the basic statistics
  tr_stats <- apply(tr, 2, sum)
  TP <- tr_stats['TP']
  TN <- tr_stats['TN']
  FP <- tr_stats['FP']
  FN <- tr_stats['FN']

  # Compute the derived statistics from the above
  total         <- TP + TN + FP + FN
  actual_pos    <- TP + FN
  actual_neg    <- TN + FP

  accuracy      <- (TP + TN) / total
  misclass_rate <- 1 - accuracy      ## aka Error Rate

  TP_rate       <- TP / actual_pos   ## aka Recall or Sensitivity
  FP_rate       <- FP / actual_neg
  TN_rate       <- TN / actual_neg
  FN_rate       <- FN / actual_pos

  specificity   <- TN / actual_neg
  precision     <- TP / (TP + FP)
  prevalence    <- actual_pos / total

  PPV             <- TP / (TP + FP)    ## Positive Predictive Value
  null_error_rate <- min(actual_neg, actual_pos) / total   ## Err Rate for guessing

  F1_score        <- 2 * (precision * TP_rate) / (precision + TP_rate)

  # Create the confusion matrix
  confusion <- matrix(data = tr_stats, nrow=2, byrow = F,
                      dimnames = list(c('Actual Pos', 'Actual Neg'),
                                      c('Predicted Pos', 'Predicted Neg')))

  actuals    <- apply(confusion, 1, sum)
  predicteds <- c(apply(confusion, 2, sum), sum(confusion))

  temp       <- t(matrix(data = c(confusion, actuals),
                         nrow = 2, byrow = FALSE))
  confusion_ext <- matrix(data = c(temp, predicteds), nrow=3, byrow = TRUE,
                          dimnames = list(c('Actual Pos', 'Actual Neg', 'Total Predicted'),
                                          c('Predicted Pos', 'Predicted Neg',
                                            'Total Actual')))

  # Prepare the data structure to return
  retval <- list(predictions      = predictions,
                 N_predictions    = nrow(predictions),
                 predictor_stats = tr_stats,
                 confusion_matrix = confusion,
                 confusion_ext    = confusion_ext,
                 total            = as.numeric(total),
                 actual_pos       = as.numeric(actual_pos),
                 actual_neg       = as.numeric(actual_neg),
                 accuracy         = as.numeric(accuracy),
                 misclass_rate    = as.numeric(misclass_rate),
                 TP_rate          = as.numeric(TP_rate),
                 FP_rate          = as.numeric(FP_rate),
                 TN_rate          = as.numeric(TN_rate),
                 FN_rate          = as.numeric(FN_rate),
                 specificity      = as.numeric(specificity),
                 precision        = as.numeric(precision),
                 recall           = as.numeric(TP_rate),
                 prevalence       = as.numeric(prevalence),
                 pos_pred_value   = as.numeric(PPV),
                 null_error_rate  = as.numeric(null_error_rate),
                 F1_score         = as.numeric(F1_score)
                 )

  return(retval)


}  #### END FUNCTION predictor_stats  ####



#----------------------------------------------------------------------------------
#  FUNCTION predictor_cor
#  ----------------------
#
#'
#' Analyzes correlations between an asset and a predictor (EMPTY)
#'
#' Computes the class correlations between an asset (or any equity curve) and
#' a predictor.  The predictor can be a binary class or a real value centered
#' about 0.
#'
#' In the real valued case, it is assumed that the sign represents the
#' direction of the market, while the real value is a conviction level.  For example,
#' a predictor value of 0.0001 could be considered a weak conviction of an up market,
#' whereas a predictor value of -0.1 could be construed as a strong conviction of a
#' down market.  Person linear correlations (function cor) is used to compute
#' the correlation between the actual future market direction (internally represented
#' as +1, -1), and the real value of the indicator
#'
#'
#'
#' @param data       The xts matrix data containing the equity curve of the asset or
#'                   portfolio, and the classifier's prediction column. e.g. market timer.
#'                   The predictor column may be binary (-1, +1) or real-valued centered
#'                   about zero.
#' @param price_col  The column name or column number of the equity curve under test.
#'                   Default = "prices".
#' @param timer_col  The column name or column number of the prediction (or timer).
#'                   Default is "GC".
#' @param plot       Logical value indicating whether or not to output a scatter plot
#'                   annotated with the class correlations and a legend.
#'
#'
#' @return  Returns the class correlations and other information as a list.
#' \describe{
#'   \item{\preformatted{$pred_N_up:}}{
#'      Number of up predictions by the predictor.
#'      }
#'   \item{\preformatted{$actual_N_up:}}{
#'      Number of actual up observations of the asset.
#'      }
#'   \item{\preformatted{$up_cor:}}{
#'      The predictor correlation for the up class (predictor value is positive).
#'      }
#'   \item{\preformatted{$pred_N_down:}}{
#'      Number of down predictions by the predictor.
#'      }
#'   \item{\preformatted{$actual_N_down:}}{
#'      Number of actual down observations of the asset.
#'      }
#'   \item{\preformatted{$down_cor:}}{
#'      The predictor correlation for the up class (predictor value is negative).
#'      }
#' }
#'
#' @export
predictor_cor <- function(data, price_col = "prices", timer_col = "GC", plot = TRUE) {


}  #####  END FUNCTION predictor_cor
