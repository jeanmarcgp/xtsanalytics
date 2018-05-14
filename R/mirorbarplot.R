###################################################################################
#
#  FUNCTIONS IN THIS FILE:  mirrorbarplot.R
#
###################################################################################


#----------------------------------------------------------------------------------
#  FUNCTION mirorbarplot
#
#' Function to create a mirrored bar plot.  This is two overlaid barplots,
#' one generally above the zero line and the other below the zero line.
#' A set of periodic returns is given (rets), along with a set of maximum
#' drawdowns (mdd).  The rets are plotted as positive bars (green) and the
#' mdd as negative bars (red). If the rets are negative, then they show up
#' as negative orange bars.
#'
#' This is normally used to show annual returns for an equity curve overlaid
#' with its associated annual max drawdown.
#'
#'
#'
#' @param rets        An xts of periodic returns, typically annual returns.
#'
#' @param mdd         An xts of periodic maximum drawdowns with the same structure
#'                    as rets.
#'
#' @param barcolors   A vector of 3 colors for the bars.  Default red, green, orange.
#'
#' @param cex.<param> These are the various cex values.  cex.lab for the Y-label size,
#'                    cex.axis for the Y-axis size, cex.legend for the legend size,
#'                    cex.names for the X-axis label size, cex.main for the title,
#'                    cex.text for the annotated text on the plot (the average line).
#'
#' @param main        The title text.
#'
#' @param legend.loc  The X,Y location of the legend.  If NULL, then no legend is plotted.
#'
#' @param yrange      The numeric range for the Y axis.  Useful to extend / shorten it.
#'                    NULL will calculate it automatically.
#'
#' @param xmgp        See par() for mgp.  This moves the title and the axises around.
#'
#' @param annotateYrs Logical. Whether to show the years on the X axis.  Note that the
#'                    period of returns should be years for this to work properly.
#'
#' @param legend.horiz Logical. Passed on as horiz to legend() and used to set
#'                     the legend horizontally rather than vertically.
#'
#' @return Nothing is returned per se.  This function creates a plot.
#'
#' @export
#----------------------------------------------------------------------------------
mirrorbarplot <- function(rets, mdd, cex.lab = 1,
                          barcolors = c("red", "green", "orange"),
                          cex.axis = 1, cex.legend = 0.8, cex.names = 1,
                          cex.main = 1,  main = "Mirror Barplot",
                          cex.text = 0.8, legend.loc = c(17, -25),
                          yrange = NULL, xmgp = c(3,1,0),
                          annotateYrs = TRUE, legend.horiz = FALSE, ...) {

  # #########
  # rets = rets[, "S&P500 (SPY)"]
  # mdd     = annualmdd[,  "S&P500 (SPY)"]
  # barcolors = c("red", "green", "orange")
  # cex.lab        = 1   #
  # cex.axis       = 1   #
  # cex.legend     = 0.8
  # cex.names      = 1   #
  # cex.main       = 1   #
  # cex.text       = 0.8
  # main           = "Mirror Barplot"
  # legend.loc     = c(17, -25)
  # annotateYrs    = TRUE
  # legend.horiz   = TRUE
  # yrange         = c(-0.4798, 0.32307)
  # xmgp           = c(3, -2, 0) #axis.title, axis.label, axis.line
  #
  # #########

  dfannualmdd   <- as.data.frame(mdd) * 100
  dfannualrets  <- as.data.frame(rets) * 100
  annualyrs     <- format(as.Date(index(mdd)), "%Y")

  dfannualrets$Gain <- ifelse(dfannualrets[, 1] > 0, dfannualrets[, 1], 0)
  dfannualrets$Loss <- ifelse(dfannualrets[, 1] < 0, dfannualrets[, 1], 0)

  yrannotations <- c(annualyrs[1:length(annualyrs)-1], paste0("YTD\n", last(annualyrs)))
  rownames(dfannualmdd)  <- yrannotations
  rownames(dfannualrets) <- yrannotations
  avgreturns    <- mean(as.numeric(dfannualrets[, 1]))
  avgdrawdown   <- mean(as.numeric(dfannualmdd[, 1]))
  print(avgdrawdown)

  # Structure the data for plotting
  mdata <- data.frame(
    group = rep(c("Gain", "Loss", "Drawdown"), each = length(yrannotations)),
    x     = rep(yrannotations, 3),
    y     = c(dfannualrets$Gain, dfannualrets$Loss, dfannualmdd[, 1])
  )


  # Create mirrored barplot
  Nbars      <- length(yrannotations) + 5


  if(is.null(yrange)) yrange <- range(mdata$y)
  textoffset <- (max(yrange) - min(yrange)) / 15 * 100
  plot(x = c(0, Nbars), yrange * 100, type = "n", ylab = "Returns (%)",
       xlab = "", xaxt = "n", bty = "n", cex.lab = cex.lab * 0.8, cex.axis = cex.axis * 0.8)

  if(annotateYrs) yrannot <- yrannotations else yrannot <- NULL # Show years on X axis?
  suppressWarnings(
    barplot(height = mdata$y[mdata$group == 'Drawdown'],add = TRUE, axes = FALSE,
            col = barcolors[1], names.arg = yrannot, cex.names = cex.names * 0.8,
            cex.main = cex.main * 0.8, main = main, mgp = xmgp)
  )

  barplot(height = mdata$y[mdata$group == 'Gain'], add = TRUE, axes = FALSE,
          col = barcolors[2])
  barplot(height = mdata$y[mdata$group == 'Loss'], add = TRUE, axes = FALSE,
          col = "white" )
  barplot(height = mdata$y[mdata$group == 'Loss'], add = TRUE, axes = FALSE,
          col = barcolors[3] )
  abline(h = 0)
  abline(h = avgreturns, lty = "dashed", col = "blue")

  if(!is.null(legend.loc)) {
    legend(x = legend.loc[1], y = legend.loc[2], cex = cex.legend,
           legend = c("Intra-Year Decline", "End of Year Gain", "End of Year Loss"),
           fill = barcolors[1:3], horiz = legend.horiz)
  }


  # Annotate average returns
  print(textoffset)
  text(x = 3.1, y = avgreturns + 2*textoffset, pos = 3, cex = cex.text * 0.8, xpd = TRUE,
       labels = colnames(dfannualrets)[1])
  text(x = 3.1, y = avgreturns + textoffset, pos = 3, cex = cex.text * 0.8, xpd = TRUE,
       labels = "Average Return")
  text(x = 3.1, y = avgreturns, pos = 3, cex = cex.text * 0.8, xpd = TRUE,
       labels = paste0(round(avgreturns, 2), "%"))

  # Annotate average drawdowns

}  ############  END FUNCTION mirrorbarplot  ##############

