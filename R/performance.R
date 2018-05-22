#
#
#  performance.R
#
#  Functions used to analyze the performance of portfolios.
#
#
#  Add DDthresh to perfstats options.
#
#  LIST OF FUNCTIONS:
#  ------------------
#
#  .perfstats         Generates performance stats from equity curves
#  .ulcerindex        A rewrite of the ulcer index to be faster.
#  .xtsdrawdowns      Multiple drawdowns
#  .xtsmdd            Efficiently computes the maximum drawdown from returns
#  .xtsCAGR           Efficiently computes the CAGR from an xts of returns
#  .plot_performance  Various complex plots to summarize EC performance
#  .rawreturns        Computes the rawreturns (not annualized) on an xts
#
#
###################################################################################
#
#   Portfolio Performance Functions
#
###################################################################################

#----------------------------------------------------------------------------------
#  FUNCTION perfstats
#
#' Compute performance statistics on equity curves
#'
#' Returns a dataframe containing performance stats on one or multiple
#' equity curves.  May also plot it on the graphic device.
#'
#' @param data     An xts matrix of equity curves, each column being one equity curve.
#' @param on       The periodicity (scale) of the xts data matrix to compute the stats.
#'                 "days", "weeks", "months", "quarters" and "years" are valid
#'                 and correspond to the following scales for table.AnnualizedReturns
#'                 respectively:  252, 52.18, 12, 4 or 1.
#' @param plotout  Logical.  Indicates whether to plot the result on the graphic console.
#'                 using function plot_df.  Default is TRUE.
#' @param value    Logical (default TRUE) to indicate whether a dataframe is returned
#' @param percent  Logical to indicate whether to multiple by 100 to get percentages
#'                 (default TRUE).
#' @param digits   Number of digits to show on the screen / file and returned dataframe.
#' @param top      The number of drawdowns to calculate, starting from the maximum drawdown.
#'
#' @param main     The title of the table to plot on the graphic console.  Default is
#'                 "Performance Summary".
#'
#' @param scaling  Table scaling factor to plot on the graphic console.  Default is "auto",
#'                 which automatically calculates a reasonable value based on the
#'                 number of columns in the xts data matrix.
#'
#' @param title_size  Title scaling factor passed on to plot_df.  Default is "auto",
#'                    which calculates a reasonable value based on the number of
#'                    coumns in the xts data matrix.
#'
#' @param ulcerthresh   The drawdown minimum threshold used in the Ulcer Performance
#'                    Index calculation for each period's return to be considered in the
#'                    calculation.  In other words, drawdowns smaller than this number
#'                    do not count.  Default is zero and it may be expressed as
#'                    either a positive or negative number.
#'
#' @param drawdown_dates  Logical.  IF TRUE, return a list containing
#'                    two dataframes.  The first is a dataframe of all performance statistics,
#'                    and the second is a dataframe of drawdown dates (Start, End, Trough)
#'                    Default is FALSE, which means only the dataframe of performance
#'                    statistics is returned.
#'
#' @param ...      Additional arguments passed on to plot_df.  For example, the title
#'                 text and size, and footnote size may be modified using this.
#'
#' @return Either a list of two dataframes (drawdown_dates is TRUE) or a single dataframe
#'        (drawdown_dates is FALSE) that shows the summary performance of the equity curves.
#'        The annualized return, the maximum drawdown and, by default the second and third
#'        drawdowns, the annualized standard deviation, the annualized Sharpe ratio (rf = 0%)
#'        the MAR ratio, the ulcer index and the percentage of positive rolling years.
#'        See parameter drawdown_dates for the specification of the list.
#'
#' @export
#----------------------------------------------------------------------------------
perfstats <- function(data, on = 'days', plotout = TRUE, value = TRUE, percent = TRUE,
                      digits = 2, top = 2, main = "Performance Summary",
                      scaling = "auto", title_size = "auto", ulcerthresh = 0,
                      drawdown_dates = FALSE, ...) {

  # #######################################
  # ######  For development
  # library(xtsanalytics)
  # data = xts_data["2008/2014", 1] #:4]
  # on   = "days";  plotout = TRUE; value = TRUE; percent = TRUE; digits = 2
  # top  = 3; main = "summary"; scaling = "auto"; title_size = "auto"
  # ulcerthresh = 0;  drawdown_dates = TRUE
  # #######################################

  opar <- par(no.readonly = TRUE)
  # Get the returns from the equity curves
  data <- data[complete.cases(data), ]
  rets <- ROC(data, type="discrete")
  rets <- rets[complete.cases(rets), ]

  # Calculate the performance statistics dataframe
  statslist <- perf_df(rets = rets, on = on, percent = percent, digits = digits,
                       top = top, ulcerthresh = ulcerthresh)

  stats_df2 <- statslist$dd_numbers

  nc        <- ncol(rets)

  # Calculate global scaling and/or title size
  # if these are set to "auto"
  if(scaling == "auto")     scaling    <- min(1, (0.1 + 8 / nc))
  if(title_size == "auto")  title_size <- min(1.8, 1.0 + nc/6)

  if(plotout) {
    footnote <- paste(index(data)[1], "/", index(data)[nrow(data)])
    if(is.na(main)) main <- "Performance Summary"

    # Shrink the footnote size if nc < 3 else default to 1
    foot_size <- min(1, nc / 3 + 0.1)

    # Too small if nc = 1, so put in title instead
    if(nc == 1) {
      main     <- paste0(main, "\n", footnote)
      footnote <- " "
    }

    plot_df(stats_df2, main, footnote = footnote, title_size = title_size,
            foot_size = foot_size, scaling = scaling, ...)

  }

  #par(opar)
  if(drawdown_dates) retlist <- list(dd_numbers = statslist$dd_numbers,
                                     dd_dates   = statslist$dd_dates)
  else
    retlist <- statslist$dd_numbers

  if(value)  return(retlist)

}  ##############  END perfstats  ##############


#---------------------------------------------------------------------------------
# Helper function perf_df - to calculate the performance dataframe
# for use in other xtsanalytics functions
#
# This function is not exported
#---------------------------------------------------------------------------------
perf_df <- function(rets, on = "days", percent = TRUE, digits = 2, top = 3,
                    ulcerthresh = 0) {


  # ################  Code for testing   ##################
  # library(xtsanalytics)
  # rets  = ROC(xts_data[, 1:2], type = "discrete")
  # rets  = rets[complete.cases(rets),]
  # on    = "days"
  # percent = TRUE
  # digits  = 2
  # top     = 3
  # ulcerthresh = 0
  # #######################################################


  # Compute basic performance stats
  switch(on,
         days = {
           stats_df <- PerformanceAnalytics::table.AnnualizedReturns(rets, scale=252, geometric=T,
                                                                     digits = digits + 2)
         },
         weeks = {
           stats_df <- PerformanceAnalytics::table.AnnualizedReturns(rets, scale=52.18, geometric=T,
                                                                     digits = digits + 2)
         },
         months = {
           stats_df <- PerformanceAnalytics::table.AnnualizedReturns(rets, scale=12, geometric=T,
                                                                     digits = digits + 2)
         },
         quarters = {
           stats_df <- PerformanceAnalytics::table.AnnualizedReturns(rets, scale=4, geometric=T,
                                                                     digits = digits + 2)
         },
         years = {
           stats_df <- PerformanceAnalytics::table.AnnualizedReturns(rets, scale=1, geometric=T,
                                                                     digits = digits + 2)
         }, {
           # Default:  stop
           stop('Value of argument on (to scale) is invalid: ', on)
         }
  )

  # Compute Ulcer Index
  ulcer    <- as.data.frame(t(ulcerindex(rets, type = "rets",
                                         on = on, DDthresh = ulcerthresh)))
  row.names(ulcer) <- "Ulcer Index"

  cagr     <- stats_df[1, ]
  upi      <- cagr / ulcer
  row.names(upi) <- "Ulcer Performance Index"

  # Convert returns to percent if enabled
  if(percent) {
    stats_df[1:2, ] <- 100 * stats_df[1:2, ]
    row.names(stats_df)[1:2] <- paste(row.names(stats_df)[1:2], "(%)")
    ulcer  <- ulcer * 100
    row.names(ulcer) <- paste(row.names(ulcer), "(%)")
  }

  # Compute all drawdowns
  # all_dd <- xtsdrawdowns(rets, top = top, digits = (digits + 2), percent = percent)

  dd_info  <- xtsdrawdowns(rets, top = top, digits = (digits + 2), percent = percent)
  all_dd   <- dd_info$DD_numbers

  #  Row bind to build the final dataframe
  rownames(stats_df)[3] <- "Annualized Sharpe"
  stats_df2 <- rbind(stats_df[1, , drop = FALSE],
                     all_dd, stats_df[2:3, , drop = FALSE ])

  #-------------------------------------------------
  # Add MAR ratio
  #-------------------------------------------------
  mar           <- -stats_df2["Annualized Return", , drop = FALSE] /
    stats_df2["Max. Drawdown", , drop = FALSE]
  rownames(mar) <- "MAR"

  #-------------------------------------------------
  # Add Positive Rolling Years
  #-------------------------------------------------
  prices    <- cumprod_na(1 + rets)
  mom252    <- make_features(prices, features = "mom252")[[1]]
  mom252    <- mom252[complete.cases(mom252), ]
  positives <- apply(mom252, 2, function(x) sum(x > 0))
  totals    <- rep(nrow(mom252), ncol(mom252))
  posroll   <- t(as.data.frame(positives / totals * 100))
  rownames(posroll) <- "Pos. Rolling Years (%)"


  #-------------------------------------------------
  # Bind then round all quantities to digits
  #-------------------------------------------------
  stats_df2 <- rbind(stats_df2, mar, ulcer, upi, posroll)
  stats_df2 <- round(stats_df2, digits = digits)

  ddlist    <- list(dd_numbers = stats_df2, dd_dates = dd_info$DD_dates)

  return(ddlist)

}




#-------------------------------------------------------------------
#' Calculate the ulcer index of equity curves
#'
#' Calculates the ulcer index of one or multiple
#' equity curves provided as an xts matrix.
#'
#' The ulcer index is implemented according to the formula
#' described in the Wikipedia article "Ulcer Index".
#'
#' @param data      An xts matrix of at least one equity curve.
#'
#' @param type      Specifies whether returns ("rets") or an equity
#'                  curve ("ec") is provided for the data.
#'
#' @param on        Specifies whether the data matrix is sampled
#'                  on "days", "weeks", "months", "quarters" or "years".
#'
#' @param DDthresh  The drawdown minimum threshold for each
#'                  period's return to be considered in the
#'                  ulcer index calculation.  In other words,
#'                  drawdowns smaller than this number do not count.
#'                  Default is zero and may be expressed as
#'                  either a positive or negative number.
#'
#'
#' @return Returns the Ulcer Performance Index for each
#'         equity curve provided.
#'
#' @export
#-------------------------------------------------------------------
ulcerindex <- function(data, type = "ec", on = "days", DDthresh = 0) {

  # ####### for testing only  #####
  # data = xts_data[, 1:4] # "SPY"]
  # type = "ec"
  # on   = "days"
  # DDthresh = 0
  # #######################


  #----------------------------------------------------------------
  # Normalize equity curves, calculate CAGR and rets
  #----------------------------------------------------------------
  if(type == "ec") {
    ec   <- data[complete.cases(data), ]
    ec[] <- apply(ec, 2, function(x) x / rep(x[1], length(x)))

    rets <- ROC(ec, type = "discrete")
    rets[is.na(rets)] <- 0

  } else {
    ec   <- cumprod_na(1 + data)
    rets <- data
  }

  N    <- nrow(ec)
  #
  # cagrfactor <- switch(on,
  #                      days     = 252,
  #                      weeks    = 52.18,
  #                      months   = 12,
  #                      quarters = 4,
  #                      years    = 1)
  #
  # cagr <- (as.numeric(last(ec))/as.numeric(first(ec)))^(cagrfactor/N) - 1

  #----------------------------------------------------------------
  # Calculate the cummax of each equity curve
  #----------------------------------------------------------------
  cmax    <- ec
  cmax[]  <- apply(cmax, 2, cummax)

  #----------------------------------------------------------------
  # Subtract the ec from cmax to get the daily drawdown matrix
  # Zero out drawdown if less than minimum threshold.
  #----------------------------------------------------------------
  dailyDD    <- (ec - cmax) / cmax
  for(i in 1:ncol(dailyDD)) {
    x <- as.numeric(dailyDD[, i])
    dailyDD[, i] <- ifelse(x < -abs(DDthresh), x, 0)
  }

  #----------------------------------------------------------------
  # Calculate Ulcer index and UPI
  #----------------------------------------------------------------
  ulcer   <- apply(dailyDD, 2, function(x) sqrt(sum(x*x)/N))
 # upi     <- cagr / ulcer

  return(ulcer)

}

#----------------------------------------------------------------------------------
#  FUNCTION xtsdrawdowns
#
#  This function is not exported
#  It is also slow because it uses table.Drawdowns
#----------------------------------------------------------------------------------
xtsdrawdowns <- function(rets, top = top, digits = 4, percent = TRUE) {

  # ################  Code for testing   ##################
  # library(xtsanalytics)
  # rets  = ROC(xts_data[, 1], type = "discrete")
  # rets  = rets[complete.cases(rets),]
  # on    = "days"
  # percent = TRUE
  # digits  = 4
  # top     = 3
  #
  # ##############
  nc    <- ncol(rets)

  #------------------------------------------------------
  # Set up the drawdown containers: df = drawdowns,
  # dflen = number of days, dffrom = DD start date
  # dfend = DD end date
  #------------------------------------------------------
  df                 <- data.frame(matrix(NA, nrow=top, ncol=nc),
                                   row.names=paste("Drawdown", 1:top))
  colnames(df)       <- colnames(rets)
  rownames(df)[1]    <- "Max. Drawdown"

  dflen              <- data.frame(matrix(NA, nrow=top, ncol=nc),
                                   row.names=paste("Drwdn", 1:top))
  colnames(dflen)    <- colnames(rets)
  rownames(dflen)[1] <- "Max. Drwdn"


  df_start    <- dflen
  df_end      <- dflen
  df_trough   <- dflen
  df_decline  <- dflen
  df_recovery <- dflen

  rownames(dflen)       <- paste(rownames(dflen),       "Days")
  rownames(df_start)    <- paste(rownames(df_start),    "Start")
  rownames(df_end)      <- paste(rownames(df_end),      "End")
  rownames(df_trough)   <- paste(rownames(df_trough),   "Trough")
  rownames(df_decline)  <- paste(rownames(df_decline),  "Decline")
  rownames(df_recovery) <- paste(rownames(df_recovery), "Recovery")


  #----------------------------------------------------------------
  # Loop through each column and extract dd and length
  # rep(0, ...) used as filler for < top drawdowns available
  #----------------------------------------------------------------
  for(i in 1:nc) {
    x          <- table.Drawdowns(rets[, i], top = top, digits = digits)
    df[, i]          <- c(x$Depth,           rep(0, top - length(x$Depth)))
    dflen[, i]       <- c(x$Length,          rep(0, top - length(x$Length)))
    df_start[, i]    <- c(x$From,            rep(0, top - length(x$From)))
    df_end[, i]      <- c(x$To,              rep(0, top - length(x$To)))
    df_trough[, i]   <- c(x$Trough,          rep(0, top - length(x$Trough)))
    df_decline[, i]  <- c(x[, "To Trough"],  rep(0, top - length(x[, "To Trough"])))
    df_recovery[, i] <- c(x$Recovery,        rep(0, top - length(x$Recovery)))
  }

  #------------------------------------------------------
  # Return dfall, interlacing df with dflen
  #------------------------------------------------------
  if(percent) {
    df <- 100 * df
    row.names(df) <- paste(row.names(df), "(%)")
  }

  dfall  <- NULL
  dfdates <- NULL

  for(i in 1:nrow(df))  {
    dfall <- rbind(dfall, df[i, , drop = FALSE ], dflen[i, , drop = FALSE],
                   df_decline[i, , drop = FALSE], df_recovery[i, , drop = FALSE])

    dfdates <- rbind(dfdates, df_start[i, , drop = FALSE], df_end[i, , drop = FALSE],
                     df_trough[i, , drop = FALSE])
  }


  dfreturn <- list(DD_numbers = dfall, DD_dates = dfdates)


  return(dfreturn)

}


#----------------------------------------------------------------------------------
#  FUNCTION xtsmdd
#
#' Compute the maximum drawdown of an xts matrix of returns.
#'
#' Returns are assumed to be geometric ("discrete").  This function
#' is very fast and works on wide xts matrices (many equity curves)
#'
#' @param rets   An xts matrix of equity curves, expressed as periodic returns
#'
#' @param digits Number of digits to report
#'
#' @return Returns a dataframe of maximum drawdowns, expressed as fractions.
#'
#'@export
#----------------------------------------------------------------------------------
xtsmdd <- function(rets, digits = 4) {
  cp = cumprod_na(1 + rets)
  cnames <- colnames(cp)
  MaxDD <- data.frame(matrix(NA, nrow=1, ncol=length(cnames)), row.names="Max. Drawdown")
  colnames(MaxDD) <- cnames
  for(i in colnames(cp)) {
    MaxDD[1,i] = min((cp[,i]/cummax(c(1, cp[,i]))[-1]) - 1)
  }

  return(round(MaxDD, digits))


}  ##########  END xtsmdd  ##########


#----------------------------------------------------------------------------------
#  FUNCTION xtsCAGR
#
#' Compute the annualized return from an xts of equity curves (prices)
#'
#' The equity curves are in the form of prices, not returns.
#'
#' @param ec   An xts matrix of equity curves expressed as prices.
#'
#' @return Returns a data frame of CAGR
#' @export
#----------------------------------------------------------------------------------
xtscagr <- function(ec) {

  freq = periodicity(ec)
  switch(freq$scale, minute = {
      stop("Data periodicity too high")
    }, hourly = {
      stop("Data periodicity too high")
    }, daily = {
      scale = 252
    }, weekly = {
      scale = 52
    }, monthly = {
      scale = 12
    }, quarterly = {
      scale = 4
    }, yearly = {
      scale = 1
    })

  Nyrs    <- nrow(ec) / scale
  total_rets <- ec[nrow(ec), ] / as.numeric(ec[1, ])
  cagr <- as.data.frame(total_rets^(1/Nyrs)) - 1

  return(cagr)

}

#----------------------------------------------------------------------------------
#  FUNCTION plot_performance
#
#' Generate combination plots to illustrate portfolio performance
#'
#' @param prices    An xts matrix of equity curves or asset prices.
#'
#' @param type      Specifies the type of plot.  If "table"
#'                  then the equity curves are shown on the top portion
#'                  and a table of key performance parameters is shown
#'                  below.  If "table2", then a simple version of table is
#'                  plotted, appropriate for showing shorter periods.
#'                  If "curves" then the equity curves
#'                  are shown on the top portion and the drawdown curves
#'                  are shown on the bottom section.  Default is
#'                  "table".
#'
#' @param main      Title at the top of the plot page
#'
#' @param log       Specifies whether to use a semilog scale for the equity
#'                  curve plot.  Default is "y" specifying the y scale is logarithmic.
#'
#' @param cex       The scaling factor for the performance table in table mode.
#'
#' @param tabheight The height of the table relative to the height of the plot, for
#'                  type = "table". Default is 0.6.
#'
#' @param labspace  The amount of space between the X and Y labels and the plot.
#'                  This is useful when scaling the chart for png plots and labels
#'                  can overlap the plot.  Used by title(), parameter line.
#'
#' @param ...       Additional parameters passed through to function xtsplot.
#'
#' @export
#-----------------------------------------------------------------------
plot_performance <- function(prices, type = "table", main = "Performance Summary",
                             log = "y", cex = 1, tabheight = 0.6, digits = 2,
                             labspace = 2, ycex = 1, ...) {

  #
  # TODO
  #  . col = "auto" in xtsplot
  #  . then, implement and recycle colset in colorset
  #  . figure out how to  rename y labels in xtsplot for mom252
  #  . test on pdf to ensure sizing is all fine.
  # main = "performance summary"
  # log = "y"
  # prices <- xtsbind(xts_data[, c(1,4,8)], xts_data[, 1:5])
  # prices <- xts_data[, 1:8, drop = FALSE]

  # ########  For testing  #######
  # library(xtsanalytics)
  # library(lubridate)
  # prices = xts_data["2012/2013", 1:4]
  # tabheight = 0.6
  # cex    = 1
  # digits = 2
  # labspace = 2
  # type   = "table2"
  # main   = "Performance Summary"
  # log    = "y"
  #
  # #####################################

  #--------------------------------------------------
  # Ensure timeframe excludes NAs
  #--------------------------------------------------
  prices <- prices[complete.cases(prices), ]
  # op <- par(no.readonly = TRUE)
  switch(type,
         table = {
           #-------------------------------------------------------------
           # Plot equity curves at top, performance table below
           #-------------------------------------------------------------
           layout(matrix(c(1, 1, 2, 3), nrow = 2, byrow = TRUE),
                  heights = c(1, tabheight), widths = 1)
           op <- par(mar = c(2.8, 5, 4, 2))

           N  <- ncol(prices)
           if(N > 8) {
             N  <- 8
             sprint("perfplot:  Too many curves to plot. Plotting first 8 columns only.")
             prices <- prices[, 1:N]
           }

           #------------------------------------------------------------
           # Override table size (cex) based on number of columns
           #------------------------------------------------------------
           if(N <= 5)      cex2 <- 1    else
             if(N == 6)    cex2 <- 0.9  else
               if(N == 7)  cex2 <- 0.8  else
                 cex2 <- 0.75


           xtsplot(prices, main = main, log = log, hline = 1.0, ...)
           #xtsplot(prices, main = main, log = log)

           pf <- perfstats(prices, plotout = FALSE, top = 3, digits = digits, percent = TRUE)
           print(pf)

           panel1_names <- c("Annualized Return (%)", "Max. Drawdown (%)", "Max. Drwdn Days",
                             "Drawdown 2 (%)", "Drwdn 2 Days")

           panel2_names <- c("Annualized Sharpe", "Annualized Std Dev (%)", "MAR",
                             "Ulcer Performance Index", "Pos. Rolling Years (%)")

           cex_table <- cex * cex2
           sprint("cex table = %s", cex_table)
           textplot(pf[panel1_names,, drop = FALSE], wrap.rownames = 23, cex = cex_table,
                    col.rownames=c("darkgreen", "red", "grey40", "red", "grey40" ),
                    cmar = 1.0, rmar = 0.1)
           textplot(pf[panel2_names,, drop = FALSE], wrap.rownames = 23,
                    col.rownames=c("darkgreen", "red", "darkgreen", "darkgreen", "darkgreen" ),
                    cmar = 1.0, rmar = 0.1, cex = cex_table)


         },
         table2 = {
           #-------------------------------------------------------------
           # Plot equity curves at top, SIMPLE performance table below
           #-------------------------------------------------------------
           layout(matrix(c(1, 2), nrow = 2, byrow = TRUE),
                  heights = c(1, tabheight), widths = 1)
           op <- par(no.readonly = TRUE)
           #op <- par(mar = c(2.8, 5, 4, 2)) #B,L,T,R

           #op <- par(mar = c(2, 5, 2, 2)) #B,L,T,R

           N  <- ncol(prices)
           if(N > 8) {
             N  <- 8
             sprint("perfplot:  Too many curves to plot. Plotting first 8 columns only.")
             prices <- prices[, 1:N]
           }

           #------------------------------------------------------------
           # Override table size (cex) based on number of columns
           #------------------------------------------------------------
           if(N <= 5)      cex2 <- 1    else
             if(N == 6)    cex2 <- 0.9  else
               if(N == 7)  cex2 <- 0.8  else
                 cex2 <- 0.75


           #xtsplot(prices, main = main, log = log, ylab = "", xlab = "",
            #       mode = "growthof100", ...)

           xtsplot(prices, main = main, log = log, ylab = "", xlab = "",
                   mode = "growthof100")
           title(ylab = "Growth of $100", line = labspace, cex.lab = ycex)

           #xtsplot(prices, main = main, log = log, hline = 1.0)

           pf <- perfstats(prices, plotout = FALSE, top = 3, digits = digits, percent = TRUE)
           print(pf)

           thisyear_n  <- year(index(last(prices)))
           lastyear_n  <- thisyear_n - 1
           lastyear    <- prices[as.character(lastyear_n), ]
           tf          <- paste0(index(last(lastyear)), "/", index(last(prices)))

           ytd_gains   <- as.numeric(last(prices[tf, ])) / as.numeric(first(prices[tf, ])) - 1
           names(ytd_gains) <- colnames(prices)

           print(pf)
           pf <- rbind(pf, data.frame(t(round(ytd_gains * 100, digits)),
                                      row.names = "YTD Return (%)"))

           panel1_names <- c("YTD Return (%)",  "Annualized Return (%)",
                             "Max. Drawdown (%)", "Annualized Std Dev (%)",
                             "Annualized Sharpe")

           cex_table <- cex * cex2
           sprint("cex table = %s", cex_table)
           textplot(pf[panel1_names,, drop = FALSE], wrap.rownames = 23, max.cex = cex_table,
                    col.rownames=c("darkgreen", "darkgreen", "red", "red", "grey40" ),
                    cmar = 1.0, rmar = 0.1, mar = c(0,0,0,0))

         },
         curves = {
           #-------------------------------------------------------------
           # Plot equity curves at top, Drawdown curve in the middle,
           # and rolling returns curve at bottom.
           #------------------------------------------------------------
           layout(matrix(c(1, 2, 3)), heights = c(2, 1, 1), widths = 1)
           op <- par(mar = c(3, 5, 4, 2))

           prices <- prices[complete.cases(prices), ]
           rets   <- ROC(prices, type = "discrete", na.pad = FALSE)

           mom252 <- make_features(prices, "mom252")[[1]] * 100

           col  <- make_colors(n = ncol(prices), type = "lines")
           xtsplot(prices, col = names(col), main = main, log = log, cex.main = 1.5,
                   legend = "topleft", hline = 1.0, ...)
           xtsplot(mom252, hline = 0, main = "12 Months Rolling Returns",
                   norm = FALSE, ylab = "Percent", xlab = "Rolling Performance",
                   legend = "topleft", col = names(col), ...)

           chart.Drawdown(rets, colorset = col, lwd = 1, cex.lab = 1.2, cex.main = 1.25,
                          cex.axis = 0.8, ylab = "",
                          main = "Drawdown from Peak Equity")
           title(ylab = "Fraction Lost", mgp = c(2.0, 1, 0))

         }, {
           # Default - stop with error
           stop("perfplot:  Invalid plot type argument.")
         })

  par(op)
  layout(matrix(c(1)), heights = c(1), widths = 1)


}


#-------------------------------------------------------------
# Function rawreturns
#
#' Compute the raw returns on an xts of prices
#'
#' @param prices   The xts matrix of prices
#'
#' @return Returns the raw returns (not annualized) returns
#' of each xts series in percentage.
#'
#' @export
#-------------------------------------------------------------
rawreturns <- function(prices) {
  x        <- (as.numeric(last(prices)) / as.numeric(first(prices)) - 1) * 100
  names(x) <- colnames(prices)
  return(x)
}

