####################################################################################
# FILE load_parameters.R
#
#
####################################################################################
# FUNCTION load_parameters
#
#' Loads parameters from an excel file to spawn multiple parametric runs
#'
#'
#' @param fname    The excel file name containing the parameters for each
#'                 parametric run.  Must include the proper file path.
#'
#' @param data     If data is provided, then it is used instead of loading
#'                 a file to prepare a dataframe of parametric runs. The
#'                 main utility of this is to do rules checking on the data
#'                 to ensure that all columns are present.  For example,
#'                 the data provided may have some missing columns but the
#'                 dataframe returned will have these columns filled with
#'                 appropriate default values.  Default is NA (read from file).
#'
#' @return Returns a dataframe, which each column properly cleaned up and
#'         containing the parameters for the runs.  Each row corresponds to
#'         a unique run.
#' @export
#-----------------------------------------------------------------------------------
load_parameters <- function(fname, data = NA) {

  #---------------------------------------------------------
  # Read the parametric excel file, delete NA rows,
  # unless data is provided.
  #---------------------------------------------------------
  if(is.na(data[1]))  dataloaded <- TRUE else
    dataloaded <- FALSE
  if(dataloaded)      dfread <- readxl::read_excel(fname) else
    dfread  <- data

  x      <- apply(dfread, 1, FUN = function(x) !all(is.na(x)))
  df     <- dfread[x, ]
  Njobs  <- nrow(df)


  #---------------------------------------------
  # Extract and clean up each job parameter
  #---------------------------------------------
  for(i in 1:Njobs) {
    if(is.null(df$runjob[i]))        df$runjob[i]        <- FALSE
    if(is.null(df$symbol[i]))        df$symbol[i]        <- NA
    if(is.null(df$datasrc[i]))       df$datasrc[i]       <- NA
    if(is.null(df$savefname[i]))     df$savefname[i]     <- NA
    if(is.null(df$loadresults[i]))   df$loadresults[i]   <- NA
    if(is.null(df$targetoffset[i]))  df$targetoffset[i]  <- NA
    if(is.null(df$timeframe[i]))     df$timeframe[i]     <- NA
    if(is.null(df$target[i]))        df$target[i]        <- NA
    if(is.null(df$targetlag[i]))     df$targetlag[i]     <- NA
    if(is.null(df$yaligncol[i]))     df$yaligncol[i]     <- NA
    if(is.null(df$smoothtarget[i]))  df$smoothtarget[i]  <- NA
    if(is.null(df$yhatsma[i]))       df$yhatsma[i]       <- NA
    if(dataloaded) {
      df$features[i]     <- list(trimspaces(unlist(strsplit(as.character(df$features[i]), ",")), type = "both"))
      if(!is.na(df$extfeatures[i]))    df$extfeatures[i]  <- list(trimspaces(unlist(
        strsplit(as.character(df$extfeatures[i]), ",")), type = "both"))

      df$featurelags[i]  <- list(as.numeric(unlist(strsplit(as.character(df$featurelags[i]), ","))))

      if(is.na(df$smooth[i])) df$smooth[i] <- NA else
        df$smooth[i]  <- as.list(trimspaces(unlist(strsplit(df$smooth[i], ",")), type = "both"))
      if(!is.na(df$smooth[i]))      names(df$smooth[i])  <- df$features[i]
      if(!is.na(df$runparallel[i])) df$runparallel[i]    <- df$runparallel[i] else
        df$runparallel[i]  <- FALSE

    }


    if(is.null(df$trade_day[i]))        df$trade_day[i]      <- NA
    if(is.null(df$wfo_span[i]))         df$wfo_span[i]       <- NA
    if(is.null(df$shaded_regions[i]))   df$shaded_regions[i] <- NA
    if(is.null(df$sub_timeframes[i]))   df$sub_timeframes[i] <- FALSE
    if(is.null(df$modelwindow[i]))      df$modelwindow[i]    <- 252
    if(is.null(df$ntree[i]))            df$ntree[i]          <- 500
    if(is.null(df$mtry[i]))             df$mtry[i]           <- 1

  }   ######  END For Loop  ######


  return(df)

}
