#
#
#  performance.R
#
#  Functions used to analyze the performance of portfolios.
#
#
#  LIST OF FUNCTIONS:
#  ------------------
#
#  .perfstats         Generates performance stats from equity curves
#  .ulcerperformance  Ulcer performance index.
#  .xtsdrawdowns      Multiple drawdowns
#  .xtsmdd            Efficiently computes the maximum drawdown from returns
#  .xtsCAGR           Efficiently computes the CAGR from an xts of returns
#  .plot_performance  Various complex plots to summarize EC performance
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
#' @param ...      Additional arguments passed on to plot_df.  For example, the title
#'                 text and size, and footnote size may be modified using this.
#'
#' @return A dataframe that shows the summary performance of the equity curves.  The
#'        annualized return, the maximum drawdown and, by default the second and third
#'        drawdowns, the annualized standard deviation, the annualized Sharpe ratio (rf = 0%)
#'        the MAR ratio, the ulcer index and the percentage of positive rolling years.
#'
#' @export
#----------------------------------------------------------------------------------
perfstats <- function(data, on = 'days', plotout = TRUE, value = TRUE, percent = TRUE,
                      digits = 2, top = 3, main = "Performance Summary",
                      scaling = "auto", title_size = "auto", ...) {

  # #######################################
  # ######  For development
  # library(xtsanalytics)
  # data = xts_data["2008/2014", 1] #:4]
  # on   = "days";  plotout = TRUE; value = TRUE; percent = TRUE; digits = 2
  # top  = 3; main = "summary"; scaling = "auto"; title_size = "auto"
  # #######################################

  # Get the returns from the equity curves
  rets <- ROC(data, type="discrete")
  rets <- rets[complete.cases(rets), ]

  # Calculate the performance statistics dataframe
  stats_df2 <- perf_df(rets, on, percent, digits, top)

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

  if(value) return(stats_df2)

}  ##############  END perfstats  ##############


#---------------------------------------------------------------------------------
# Helper function perf_df - to calculate the performance dataframe
# for use in other xtsanalytics functions
#
# This function is not exported
#---------------------------------------------------------------------------------
perf_df <- function(rets, on = "days", percent = TRUE, digits = 2, top = 3) {


  # ################  Code for testing   ##################
  # library(xtsanalytics)
  # rets  = ROC(xts_data[, 1:2], type = "discrete")
  # rets  = rets[complete.cases(rets),]
  # on    = "days"
  # percent = TRUE
  # digits  = 2
  # top     = 3
  #
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

  # Compute Ulcer Performance Index
  upi      <- as.data.frame(t(ulcerperformance(rets, type = "rets")))
  row.names(upi) <- "Ulcer Perf. Index"

  # Convert returns to percent if enabled
  if(percent) {
    stats_df[1:2, ] <- 100 * stats_df[1:2, ]
    row.names(stats_df)[1:2] <- paste(row.names(stats_df)[1:2], "(%)")
  }

  # Compute all drawdowns
  all_dd <- xtsdrawdowns(rets, top = top, digits = (digits + 2), percent = percent)

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
  stats_df2 <- rbind(stats_df2, mar, upi, posroll)
  stats_df2 <- round(stats_df2, digits = digits)

  return(stats_df2)

}




#-------------------------------------------------------------------
#' Calculate the Ulcer Performance Index of equity curves
#'
#' Calculates the Ulcer Performance Index of one or multiple
#' equity curves provided as an xts matrix.
#'
#' UPi is implemented using the formula described in the
#' Wikipedia article "Ulcer Index" and UPI.
#'
#' @param data  An xts matrix of at least one equity curve.
#'
#' @param type  Specifies whether returns ("rets") or an equity
#'              curve ("ec") is provided for the data.
#'
#' @return Returns the Ulcer Performance Index for each
#'         equity curve provided.
#'
#' @export
#-------------------------------------------------------------------
ulcerperformance <- function(data, type = "ec") {

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
  cagr <- (as.numeric(last(ec))/as.numeric(first(ec)))^(252/N) - 1

  #----------------------------------------------------------------
  # Calculate the cummax of each equity curve
  #----------------------------------------------------------------
  cmax    <- ec
  cmax[]  <- apply(cmax, 2, cummax)

  #----------------------------------------------------------------
  # Subtract the ec from cmax to get the daily drawdown matrix
  #----------------------------------------------------------------
  dailyDD <- ec - cmax

  #----------------------------------------------------------------
  # Calculate Ulcer index and UPI
  #----------------------------------------------------------------
  ulcer   <- apply(dailyDD, 2, function(x) sqrt(sum(x*x)/N))
  upi     <- cagr / ulcer

  return(upi)

}

#----------------------------------------------------------------------------------
#  FUNCTION xtsdrawdowns
#
#  This function is not exported
#  It is also slow because it uses table.Drawdowns
#----------------------------------------------------------------------------------
xtsdrawdowns <- function(rets, top = top, digits = 4, percent = TRUE) {

  # ############# for code testing  ############
  # ################  Code for testing   ##################
  # library(xtsanalytics)
  # rets  = ROC(xts_data[, 1], type = "discrete")
  # rets  = rets[complete.cases(rets),]
  # on    = "days"
  # percent = TRUE
  # digits  = 2
  # top     = 3
  #
  # ##############
  nc    <- ncol(rets)

  #------------------------------------------------------
  # Set up the DD and DDlength containers
  #------------------------------------------------------
  df                 <- data.frame(matrix(NA, nrow=top, ncol=nc),
                                   row.names=paste("Drawdown", 1:top))
  colnames(df)       <- colnames(rets)
  rownames(df)[1]    <- "Max. Drawdown"

  dflen              <- data.frame(matrix(NA, nrow=top, ncol=nc),
                                   row.names=paste("Drwdn", 1:top, "Days"))
  colnames(dflen)    <- colnames(rets)
  rownames(dflen)[1] <- "Max. Drwdn Days"

  #------------------------------------------------------
  # Loop through each column and extract dd and length
  #------------------------------------------------------
  for(i in 1:nc) {
    x          <- table.Drawdowns(rets[, i], top = top, digits = digits)
    df[, i]    <- c(x$Depth,  rep(0, top - length(x$Depth)))
    dflen[, i] <- c(x$Length, rep(0, top - length(x$Length)))
  }

  #------------------------------------------------------
  # Return dfall, interlacing df with dflen
  #------------------------------------------------------
  if(percent) {
    df <- 100 * df
    row.names(df) <- paste(row.names(df), "(%)")
  }

  dfall  <- NULL
  for(i in 1:nrow(df)) dfall <- rbind(dfall, df[i, , drop = FALSE ], dflen[i, , drop = FALSE])

  return(dfall)

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
#'                  below.  If "curves" then the equity curves
#'                  are shown on the top portion and the drawdown curves
#'                  are shown on the bottom section.  Default is
#'                  "table".
#'
#' @param main      Title at the top of the plot page
#'
#' @param log       Specifies whether to use a semilog scale for the equity
#'                  curve plot.  Default is "y" specifying the y scale is logarithmic.
#'
#' @param ...       Additional parameters passed through to function xtsplot.
#'
#' @export
#-----------------------------------------------------------------------
plot_performance <- function(prices, type = "table", main = "Performance Summary",
                             log = "y", ...) {

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
  # prices = xts_data[, 1:4]
  # type   = "table"
  # main   = "Performance Summary"
  # log    = "y"
  #
  # #####################################

  op <- par(no.readonly = TRUE)
  switch(type,
         table = {
           #-------------------------------------------------------------
           # Plot equity curves at top, performance table below
           #-------------------------------------------------------------
           layout(matrix(c(1, 1, 2, 3), nrow = 2, byrow = TRUE),
                  heights = c(2, 1, 1), widths = 1)
           par(mar = c(2.8, 5, 4, 2))


           N  <- ncol(prices)
           if(N > 8) {
             N  <- 8
             sprint("perfplot:  Too many curves to plot. Plotting first 8 columns only.")
             prices <- prices[, 1:N]
           }

           #------------------------------------------------------------
           # Override table size (cex) based on number of columns
           #------------------------------------------------------------
           if(N <= 5)      cex <- 1    else
             if(N == 6)    cex <- 0.9  else
               if(N == 7)  cex <- 0.8  else
                 cex <- 0.75


           #xtsplot(prices, main = main, log = log, hline = 1.0, ...)
           xtsplot(prices, main = main, log = log)

           pf <- perfstats(prices, plotout = FALSE, top = 3, digits = 2, percent = TRUE)
           print(pf)

           panel1_names <- c("Annualized Return (%)", "Max. Drawdown (%)", "Max. Drwdn Days",
                             "Drawdown 2 (%)", "Drwdn 2 Days")

           panel2_names <- c("Annualized Sharpe", "Annualized Std Dev (%)", "MAR",
                             "Ulcer Perf. Index", "Pos. Rolling Years (%)")

           textplot(pf[panel1_names,, drop = FALSE], wrap.rownames = 23, cex = cex,
                    col.rownames=c("darkgreen", "red", "grey40", "red", "grey40" ),
                    cmar = 1.0, rmar = 0.1)
           textplot(pf[panel2_names,, drop = FALSE], wrap.rownames = 23,
                    col.rownames=c("darkgreen", "red", "darkgreen", "darkgreen", "darkgreen" ),
                    cmar = 1.0, rmar = 0.1, cex = cex)


         },
         curves = {
           #-------------------------------------------------------------
           # Plot equity curves at top, Drawdown curve in the middle,
           # and rolling returns curve at bottom.
           #------------------------------------------------------------
           layout(matrix(c(1, 2, 3)), heights = c(2, 1, 1), widths = 1)
           par(mar = c(3, 5, 4, 2))

           prices <- prices[complete.cases(prices), ]
           rets   <- ROC(prices, type = "discrete", na.pad = FALSE)

           mom252 <- make_features(prices, "mom252")[[1]] * 100

           col  <- make_colors(n = ncol(prices), type = "lines")
           xtsplot(prices, col = names(col), main = main, log = log, cex.main = 1.5,
                   legend = "topleft", hline = 1.0, ...)
           xtsplot(mom252, hline = 0, main = "12 Months Rolling Returns",
                   norm = FALSE, ylab = "Percent", xlab = "Rolling Performance",
                   legend = "topleft", col = names(col))

           chart.Drawdown(rets, colorset = col, lwd = 1, cex.lab = 1.2, cex.main = 1.25,
                          cex.axis = 0.8, ylab = "",
                          main = "Drawdown from Peak Equity")
           title(ylab = "Fraction Lost", mgp = c(2.0, 1, 0))

         }, {
           # Default - stop with error
           stop("perfplot:  Invalid plot type argument.")
         })



  par(op)


}
