
####################################################################################
# FILE wfo_timeframe.R
#
#
####################################################################################
# FUNCTION wfo_timeframe
#
#' Calculate the WFO dates and subset the feature matrix
#'
#' @param featuremat    An xts matrix containing the features for the WFO model
#'
#' @param modelwindow   The size, in days, for the WFO model training window.
#'
#' @param wfo_span      The span for each WFO model or a character vector of dates.
#'                      Valid spans include "days",
#'                      "weeks", "months" or "quarters".  Uses function endpoints()
#'                      to calculate the model dates.
#'
#' @param ylag          An additional offset to add to the model window to adjust
#'                      for the amount of time the predicted variable (y) is lagged
#'                      in the WFO model.
#'
#' @param earliest      The earliest possible wfo date desired.  This is useful if
#'                      the featuremat goes much further back in time than needed.
#'
#' @param latest        The latest date wfo or a prediction is desired.  This is used
#'                      to truncate the wfo dates and also as the timeframe last date.
#'
#' @param verbose       If TRUE, the wfo_span dates will be printed on the console.
#'
#' @return   Returns a list of the following elements:
#' \describe{
#'   \item{\preformatted{$wfo_dates}}{
#'      A character vector containing all WFO dates.
#'      }
#'
#'   \item{\preformatted{$wfo_points}}{
#'      A numeric vector containing all WFO indices, as referred to featuremat.
#'      }
#'
#'   \item{\preformatted{$wfo_start_date}}{
#'      The first WFO date.  Identical to $wfo_dates[1].
#'      }
#'
#'   \item{\preformatted{$wfo_last_date}}{
#'      The last WFO date, which should also be the last available date in featuremat.
#'      }
#'
#'   \item{\preformatted{$wfo_timeframe}}{
#'      The WFO timeframe, which is $wfo_start_date up to and including $wfo_last_date.
#'      Do NOT use this to subset the featuremat!
#'      }
#'
#'    \item{\preformatted{$subset_timeframe}}{
#'      The timeframe that can be used to subset the featuremat matrix.  The first date
#'      is properly adjusted using modelwindow and ylag.  The last date is identical to
#'      $wfo_last_date.
#'      }
#'
#'    \item{\preformatted{$Nlast}}{
#'      This the numeric index associated with the wfo_last_date. It can be used
#'      when numeric indexing on featuremat is preferable.
#'      }
#'
#'    \item{\preformatted{$warn_msg}}{
#'      A character string containing a warning message if the earliest date cannot
#'      be met due to lack of data in featuremat.
#'      }
#'
#'   }
#'
#' @export
#-----------------------------------------------------------------------------------
wfo_timeframe <- function(featuremat, modelwindow = 252, wfo_span = "months",
                          ylag = 21, earliest = NA, latest = NA, verbose = FALSE) {


  valid_span  <- c("days", "weeks", "months", "quarters")

  if(length(wfo_span) == 1 && wfo_span %in% valid_span) {
    # Extract the wfo_span date index and remove 0
    wfo_points  <- endpoints(featuremat, on = wfo_span)[-1]
  } else {
    # Convert wfo_span dates specified as argument into equivalent indices
    wfo_points  <- featuremat[wfo_span, , which.i = TRUE]
  }


  #---------------------------------------------------------
  # Ensure wfo_points fall within the bounds of featuremat
  # Nlast is last prediction date and a prediction
  # date to return a predictive model if needed?
  #---------------------------------------------------------

  if(!is.na(earliest)) {
    earliest_i <- featuremat[paste0(earliest, "/"), , which.i = TRUE][1] - 1
    wfo_points <- wfo_points[wfo_points >= earliest_i]
  }


  if(!is.na(latest)) {
    latest_i   <- last(featuremat[paste0("/", latest), , which.i = TRUE])
    wfo_points <- wfo_points[wfo_points <= latest_i]
  }


  start_win  <- wfo_points - modelwindow + 1 - ylag
  if(start_win[1] < 1) warning_flag <- TRUE else warning_flag <- FALSE

  wfo_points <- wfo_points[start_win > 0]
  start_win  <- start_win[start_win > 0]

  Npoints    <- length(wfo_points)
  Nlast      <- wfo_points[Npoints]

  wfo_dates      <- index(featuremat[wfo_points, ])
  wfo_start_date <- wfo_dates[1]
  wfo_last_date   <- wfo_dates[Npoints]

  start_win_date  <- index(featuremat[start_win[1], ])
  subset_tf       <- paste0(start_win_date, "/", wfo_last_date)

  if(verbose) {
    sprint("Data subsetting timeframe: %s", subset_tf)
    sprint("WFO span dates: %s", str_c(wfo_dates, collapse = ", "))
  }

  if(warning_flag) {
    warn_msg <- paste("WARNING: WFO timeframe starts too early.",
                      "New start date set to:", wfo_start_date)
  } else warn_msg <- NA

  #---------------------------------------------------------
  # Build the list to return
  #---------------------------------------------------------
  rlist <- list(wfo_dates        = wfo_dates,
                wfo_points       = wfo_points,
                wfo_start_date   = wfo_start_date,
                wfo_last_date    = wfo_last_date,
                wfo_timeframe    = paste0(wfo_start_date, "/", wfo_last_date),
                subset_timeframe = subset_tf,
                Nlast            = Nlast,
                warn_msg         = warn_msg
                )

  return(rlist)


}
