####################################################################################
# FILE subset_dfdates.R
#
# Functions in this file
#  .subset_dfdates
#
####################################################################################
#
#' Subsets a dataframe by dates
#'
#' The dataframe argument must contain a column with valid dates.  The
#' timeframe argument specifies a timeframe xts-style daily.  For example,
#' "2011-01-01/2011-12-31" would be value whereas "2011" is not.
#'
#' @param df         The dataframe to subset by dates
#'
#' @param timeframe  A character string specifying the timeframe as two
#'                   full dates separated by a slash.  For example,
#'                   "2010-10-01/2010-10-22".
#'
#' @param datecol    The column name where the dates are found in the dataframe.
#'                   Although these may be of class char, they will be converted
#'                   to class Date before subsetting is applied.
#'
#' @param keep_NAs   Logical flag specifying whether to apply complete.cases
#'                   to the dataframe to eliminate rows with NAs.
#'
#' @return  Returns a subsetted dataframe containing rows with dates within the
#'          timeframe supplied.  Also, complete.cases is applied to eliminate
#'          rows with NAs if keep_NAs = FALSE (default).
#'
#' @export
#-----------------------------------------------------------------------------------
subset_dfdates <- function(df, timeframe, datecol, keep_NAs = FALSE) {


  cnames  <- colnames(df)
  ndate   <- as.numeric(as.Date(df[, datecol]))
  df      <- cbind(ndate, df)
  df      <- df[order(df$ndate), ]  # sort in ascending dates

  datevec <- as.Date(unlist(str_split(timeframe, "/")))
  datenum <- as.numeric(datevec)

  dfsub   <- df[(df$ndate >= datenum[1] & df$ndate <= datenum[2]), ]
  dfsub   <- dfsub[, cnames]

  if(!keep_NAs) dfsub <- dfsub[complete.cases(dfsub), ]

  return(dfsub)

}  ############  END FUNCTION subset_dfdates  ##############




