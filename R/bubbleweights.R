#############################################
#
#  FILE bubbleweights.R
#  --------------------
#
#' Generate a bubble plot of asset weights.
#'
#' This function is useful to visualize the momentm vs. volatility of a universe
#' of assets, along with the relative weights of each asset.  The colors of each
#' bubble dot is chosen based on the asset class for the given dot.
#'
#'
#' @param  x           A named vector or single row xts containing the X values.  If a
#'                     single row xts, the index is ignored, but the column names are
#'                     used to name any unnamed vector provided for y or dotweights.
#'
#' @param  y           An order-matched unnamed vector, or a single row xts containing
#'                     the Y values.
#'
#' @param dotweights   An order-matched vector or single row xts containing the relative weights
#'                     of each dots.
#'
#' @param dotclass     A list associating each asset to its asset class.  The list follows the
#'                     format asset_name = "asset class" e.g. SPY = "equities".  Up to TBD asset
#'                     classes can be named with colors automatically assigned.
#'
#' @param labelpos     A named vector or a list of labels associating some or all assets
#'                     to a non-default location relative to the dot on the plot.  The default
#'                     label location is on the right.  Valid locations are
#'                     "above", "below", "left" or "right".  Format for the list is:
#'                     SPY = "above". Default is NA i.e. no exceptions, all labels on the right.
#'
#' @param xlab         The x-axis label.
#'
#' @param ylab         The y-axis label.
#'
#' @param legendloc    The legend location.  Valid values are "bottomright", "bottom",
#'                     "bottomleft", "left", "topleft", "top", "topright", "right" and "center".
#'
#' @param hline        The vertical location to place a horizontal line.  Default is NA (no line).
#'
#' @param cex.dots     Relative scaling of the labels on each dot.
#'
#' @param cex.legend   Relative scaling of the legend.
#'
#' @param main         The plot title.  The index(last(x)) is automatically appended
#'                     and plotted under the title to show the date of the analysis.
#'
#' @param ...          Additional parameters passed through to function plot.
#'
#'
#' @export
#---------------------------------------------------------------------------------
bubbleweights <- function(x, y, dotweights, dotclass = NA, labelpos = NA,
                          xlab = "Volatility (%)", ylab = "Momentum (%)", legendloc = "topleft",
                          cex.dots = 0.8, cex.legend = 0.8,
                          hline = NA, main = "Momentum vs. Volatility Bubble Plot", ...) {

  # ###################################
  # # To Test function
  # library(xtsanalytics)
  # x          = last(xts_data)/sum(last(xts_data))
  # y          = xts_data[600, ]/sum(xts_data[600, ])
  # xlab       = "Volatility (%)"
  # ylab       = "Momentum (%)"
  # dotweights = x *2 + 0.1
  # dotweights[1, "EWN"] = 0
  #
  # main       = "Momentum vs. Volatility Bubble Plot"
  #
  # dotclass   = NA
  # dotclass   = list(SPY = "equities",       VTI = "Equities",      BND = "bonds",
  #                   VNQ = "Real Asset",     QQQ = "Cash Asset",    VIG = "equities",
  #                   IEV = "stocks", EWN = "stocks")
  #
  # labelpos   = NA
  # labelpos   = list(QQQ = "above")
  # labelpos   = list(BND = "left", VTI = "below", SPY = "above", EWN = "right", QQQ = "below")
  # ##########

  #------------------------------------------------------
  # Condition the function arguments and extract
  # relevant information from them
  #------------------------------------------------------
  if(is.xts(x)) {
    x          <- last(x)
    cnames     <- colnames(x)
    bubbledate <- as.Date(index(x))
    main       <- paste0(main, "\nAs of ", bubbledate)
  } else {
    cnames     <- names(x)
    bubbledate <- ""
  }

  stopifnot(length(cnames) == length(x))
  nassets <- length(cnames)

  if(is.vector(y)) names(y) <- cnames

  dotweights <- as.vector(dotweights)
  dotweights <- recycle_better(dotweights, cnames)
  dotweights <- dotweights / sum(dotweights)   # Normalize to sum to one

  if(!all(cnames %in% names(dotclass)))
    stop("Must provide an asset class for each asset provided.")

  dotclass <- unlist(dotclass)
  dotclass[dotclass == "equities"]     <- "Equities"
  dotclass[dotclass == "stocks"]       <- "Equities"
  dotclass[dotclass == "fixed income"] <- "Fixed Income"
  dotclass[dotclass == "bonds"]        <- "Fixed Income"
  dotclass[dotclass == "cash asset"]   <- "Cash Asset"
  dotclass[dotclass == "real asset"]   <- "Real Asset"

  #-----------------------------------------------------
  # Figure out the non-default dot positions
  #-----------------------------------------------------
  labelposn          <- c(below = 1, left = 2, above = 3, right = 4)
  labelposvec        <- rep(4, nassets)
  names(labelposvec) <- cnames
  labelposexcep      <- unlist(labelpos)
  stopifnot(labelposexcep %in% c(names(labelposn), NA))

  # Map the position name to number for each exception
  labelposvec[names(labelposexcep)] <- labelposn[labelposexcep]

  #---------------------------------------------
  # Generate the Bubble plot
  #---------------------------------------------
  xp      <- as.numeric(x)
  yp      <- as.numeric(y)
  dottype <- ifelse(dotweights == 0 , 1, 21)
  dsize   <- 1 + 8 * dotweights
  yextra  <- (max(yp) - min(yp)) * 0.1
  yext    <- c(-yextra, yextra)
  ylim    <- range(yp) + yext

  xextra  <- (max(xp) - min(xp)) * 0.1
  xext    <- c(-xextra, xextra)
  xlim    <- range(xp) + xext

  assetclasses  <- c("Equities", "Fixed Income", "Real Asset", "Cash Asset")
  no_classes    <- dotclass[!(dotclass %in% assetclasses)]
  if(length(no_classes) != 0) {
    sprint("The following assets have no recognized classes.  Coloring in RED:")
    print(names(no_classes))
    dotclass[names(no_classes)] <- "Unassigned"
  }

  dcol <- lcol <- dotclass
  dcol[dcol == "Equities"]     <- "deepskyblue3"   # dot fill color
  lcol[lcol == "Equities"]     <- "blue"           # line color

  dcol[dcol == "Fixed Income"] <- "palegreen2"     # dot fill color
  lcol[lcol == "Fixed Income"] <- "darkgreen"      # line color

  dcol[dcol == "Real Asset"]   <- "purple"       # dot fill color
  lcol[lcol == "Real Asset"]   <- "purple4"      # line color

  dcol[dcol == "Cash Asset"]   <- "grey60"      # dot fill color
  lcol[lcol == "Cash Asset"]   <- "black"       # line color

  dcol[dcol == "Unassigned"]   <- "red"      # dot fill color
  lcol[lcol == "Unassigned"]   <- "red4"       # line color

  #print(x)
  #print(y)

  plot(x = xp, y = yp, xlab = xlab, ylab = ylab, xlim = xlim, ylim = ylim, lwd = 2,
       pch = dottype, cex = dsize, main = main, bg = dcol, col = lcol, ...)

  if(!is.na(hline)) abline(h = hline, lwd = 1, lty = "dotted", col = "blue")

  #----------------------------------------------------------
  # Add asset name labels
  #----------------------------------------------------------
  toffset <- 0.4 + 1.5 * dotweights
  for(i in 1:nassets) {
    text(x = xp[i], y = yp[i], labels = cnames[i], pos = labelposvec[i],
         offset = toffset[i], cex = cex.dots)
  }

  #----------------------------------------------------------
  # Asset asset class labels with relative percentages
  #----------------------------------------------------------
  legendcols <- c(Equities = "deepskyblue3", `Fixed Income` = "palegreen2",
                  `Real Asset` = "purple", `Cash Asset` = "grey60", Unassigned = "red")
  legenditems <- legendcols[names(legendcols) %in% unique(dotclass)]

  # Compute asset class percentages
  assetpct  <- unlist(lapply(names(legenditems), function(x) sum(dotweights[dotclass == x])))
  names(assetpct)    <- names(legenditems)
  names(legenditems) <- paste0(names(legenditems), " (", round(assetpct*100, 1), "%)")

  # Plot legend
  legend(legendloc, cex = cex.legend, legend = names(legenditems), fill = legenditems)


}  #########  END FUNCTION bubbleweight  ##########


