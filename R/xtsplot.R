#####################################################################
#
#  xtsplot.R
#
#  FUNCTIONS in this file:
#  . xtsplot              (exported)
#  . make_colors          (exported)
#  . get_shaded_regions   (Not Exported)
#
#####################################################################
#####################################################################
#------------------------------------------------------------------------------------
#'
#' Plot xts time series objects via multiple methods.
#'
#' This function is a wrapper for plot.zoo and plot.xts by providing
#' a simple, unified way to plot time series to the screen or to a png file.
#' It includes nice defaults for colors and legend formatting.
#'
#' In addition, xtsplot provides the following facilities:
#'
#'
#' \itemize{
#'   \item Normalization of curves so they all start at the same time and
#'     the same level.
#'   \item Shaded regions in the plot when argument shaded contains an xts
#'     matrix.  See shaded and shaded_col for details.
#'   \item A horizontal and/or a vertical line can be added to the plot to
#'     show a crosshair or highlight a given level or time using vline and hline.
#'   \item Plot can be automatically saved to a file using a time stamp, and is
#'     designed to look essentially identical to the screen version.
#'   \item A legend can be automatically added and placed at the best location on
#'     the plot.
#'   \item For multiple line plots, one curve may be selected as the benchmark and
#'     appropriately plotted last (last on the legend also) so it's on top of the others.
#'     By default, its color is black and lwd = 3.
#'   \item Log scale are allowed for all plot options.
#'
#'   }
#'
#' @param data       An xts matrix containing one or multiple columns to plot.
#'
#' @param datarange  An xts matrix of similar size as data, used only for the purpose
#'                   of setting a plot labeling range that differs from data.  This
#'                   is normally used to scale the y axis to allow easy side-by-side
#'                   comparison of two similar plots.  Default is NA, which means it is
#'                   ignored.
#'
#' @param method     The plot method to use.  Must be one of "zoo", "xts", or "custom"
#' @param ptype      The type of plot to display.  Must be one of "equity_curve" or
#'                   "performance". An equity curve plot displays a single plot
#'                   region with all equity curves. A performance plot displays
#'                   3 regions:  an equity curve plot, a 12 month rolling return
#'                   plot and a drawdown plot. (NOT IMPLEMENTED)
#' @param legend     Location of the legend on the plot, or "none" if no legend is
#'                   desired.  Must be one of "none", "bottomright", "bottom",
#'                   "bottomleft", "left", "topleft", "top", "topright", "right"
#'                   or "center".  Default is "topleft". This argument is passed
#'                   to the plot method.
#'
#' @param norm       Logical. Flag specifying whether to normalize multiple
#'                   curves to a common starting date.  When set to FALSE,
#'                   nothing is normalized and rows with all NAs (including leading rows)
#'                   are shown on the time scale (as blank curves).  When set to TRUE,
#'                   then rows with all NAs are removed as a first step.  Then, if
#'                   tie_recent is NA, all rows with some NAs are also removed so that
#'                   all curves can start on the same date.  On the other hand, if
#'                   tie_recent is set to a given column, then all curves starting later than the
#'                   earliest curve are plotted on their starting date (the earliest curve
#'                   is not truncated), and these curves are tied to the benchmark or
#'                   column 1 if no benchmark is specified.
#'
#' @param mode       Defines the mode used to plot the information.  When mode = "gain",
#'                   the percentage gain is shown starting at 0% (and thefore normalized),
#'                   but only a linear scale is allowed.  When mode = "portfolio" (the default),
#'                   then the value of the portfolio is shown starting at 1.0 if normalized is
#'                   true.  When mode = "growthof100", then all is normalized at 100.
#' @param bench      Specifies the column (number or name) to highlight as the benchmark.
#' @param log        Log argument passed to methods.  Currently supported by the
#'                   zoo method, and its value can be "y" (for y log scale), "x"
#'                   (for x log scale) or "xy" for both.
#'
#' @param fname      If NULL (default), then only plot to the screen.  If fname is
#'                   provided, then create a png file at the given path in addition
#'                   to plotting on the screen. The output file name is fname
#'                   with a date-time stamp appended.
#'
#' @param pngsize    A length 2 vector containing the size of the png plot file,
#'                   specified in pixels as c(x, y).
#' @param col        Vector of color names to use when plotting lines. The colors should be part
#'                   of the set: c('black', 'blue', 'green', 'red', 'orange', 'purple', 'brown',
#'                   'darkpink', 'grey', 'turquoise', 'mauve', 'lightblue', 'lightgreen', 'pink',
#'                   'lightorange', 'lightpurple', 'yellow').  This is normally used to change
#'                   the order of colors for plots with multiple lines. Base R colors can
#'                   also be used if specified.  Automatically recycled.
#'
#' @param lwd        Vector specifying the line width.  Default is 1. Recycled as needed.
#' @param lty        Vector specifying the line type.  Default is "line" or 1.  Recycled. See par()
#'                   for additional details on lwd and lty.
#' @param shaded     An logical (or 0, 1) xts matrix specifying the shaded regions.
#'                   Each column is associated with a region type (color).  When a
#'                   row contains true, that date is shaded.
#'
#' @param shaded_inv Logical flag indicating whether to invert the xts matrix shaded
#'                   before plotting the regions. This is helpful when plotting market
#'                   timers where normally true means a bull market but we want to highlight
#'                   false (the bear markets).
#' @param shaded_col A vector specifying the shaded regions colors.  This vector is
#'                   recycled if there are more types of regions than colors.  The colors
#'                   must be part of the set: c('green', 'red', 'orange', 'grey', 'blue',
#'                   'yellow', 'purple', 'brown', 'pink').  These colors are not the same
#'                   as the line colors as they are purposely made pastel-like and transparent
#'                   to highlight overlapping regions.  Recycled as needed.
#'
#' @param vline      Adds a vertical line to the plot. May be a vector of length 1 or
#'                   a list of length 2 or 3.  NA = no line plotted (default). First list
#'                   element (or length one vector) contains the
#'                   X coordinate of the vertical line, and it is either a numeric
#'                   for a zoo plot, or a string representing a date for an xts plot.
#'                   Second list element (optional) is the color of the line in either
#'                   numeric form or a string with the color name, which must correspond
#'                   to a color in argument cols.  The third list element specifies the line width
#'                   or, if not specified, the line width defaults to lwd = 1.
#'
#' @param vlabel     Adds a vertical text label on the vline, if specified.  vlabel is
#'                   specified as a list of two items.  Item 1 specifies the y value
#'                   where the label will be placed (label ends at that y value). The size of
#'                   the text follows cex.legend. Item 2 specifies the actual label text.
#'                   The label is placed on the left side of the vline, reading from bottom to
#'                   the top.
#'
#' @param hline      Adds a horizontal line to the plot.  Arguments follow a similar format
#'                   as vline.
#'
#' @param xlab       X-axis label as a character string.  If omitted, then default is
#'                   the plotted timeframe, possibly adjusted by omitting leading NAs.
#'
#' @param ylab       Y-axis label as a character string.  If omitted, then default is
#'                   "Prices" or, if data is normalized, then "Norm. Prices".
#'
#' @param cex.legend The relative size for the legend and the vlabel.  Default is 0.7.
#'
#' @param cex.lab    The relative size for the axis labels.  Default is 1.15.
#'
#' @param mgp        The margin line for the axis title, axis lables and axis line.
#'                   See help for par().
#'
#' @param return_xts Logical. When TRUE, the normalized and scaled xts matrix plotted
#'                   is returned.  Default is FALSE.
#'
#' @param tie_recent Default is NA. When set to a column number or name in the data matrix,
#'                   all curves that have a starting date more
#'                   recent than the benchmark will be tied (that is, attached) to the
#'                   equity curve represented by that column on their first trading day.
#'                   This allows easy visualizations of recent performance by direct
#'                   overlap of these curves.
#'
#' @param ...        Additional arguments passed to the plot method.
#'
#'
#'
#' @export
#-----------------------------------------------------------------------------------
xtsplot <- function(data,
                    datarange   = NA,
                    method      = c('zoo', 'xts', 'custom'),
                    ptype        = c("equity_curve", "performance"),
                    legend      = "topleft",
                    norm        = TRUE,
                    mode        = "portfolio",
                    bench       = 0,
                    log         = "",
                    fname       = NULL,
                    pngsize     = c(1280, 720),
                    col         = "auto",
                    lwd         = 1,
                    lty         = 1,
                    shaded      = NULL,
                    shaded_inv  = TRUE,
                    shaded_col  = c('green', 'red', 'orange', 'grey', 'blue',
                                    'yellow', 'purple', 'brown', 'pink' ),
                    vline       = NA,
                    vlabel      = NA,
                    hline       = NA,
                    xlab        = NA,
                    ylab        = NA,
                    cex.legend  = 0.7,
                    cex.lab     = 1.15,
                    mgp         = c(1.8, 0.6, 0),
                    return_xts  = FALSE,
                    tie_recent  = NA,
                     ... ) {

  # ################################################
  # ####  For function devel only  ##
  # data   = xts_data[, c(1,4,8)]
  # method = 'zoo'; legend = "topleft"; norm = TRUE; bench = 0
  # log = "y"; fname = NULL; pngsize = c(1280, 720)
  # col = "auto"
  # lwd = 1; lty = 1; shaded = NULL; hline = 0
  # #################################################

 # rets   <- ROC(prices, type = "discrete")
#  mom252 <- make_features(prices, "mom252")[[1]]



  #------------------------------------------------------------
  # Basic test to make sure arguments are set up right
  #------------------------------------------------------------
  if(!is.null(shaded)) {
    if(ncol(shaded) == 0) stop("xtsplot: arg shaded empty!")
  }

  #------------------------------------------------------------
  # Initial setup:  par, get arguments pairlist
  #------------------------------------------------------------
 # save_par <- par(no.readonly=TRUE)
  par(xaxs="i")                        # Remove 4% space on both sides

  #  Create a pairlist of all dotted arguments to refer for png output
  alldots <- match.call(expand.dots = F)$`...`

  sprint("\nxtsplot function call:")

  #------------------------------------------------------------
  # Normalize the curves unless specified otherwise
  #------------------------------------------------------------
  if(norm && is.na(tie_recent)) {

    data <- data[complete.cases(data), , drop=FALSE]


    if(any(as.numeric(data[1,]) == 0))
      stop("xtsplot:  Can't normalize. First data row contains zeroes.")
    coredata(data) <- apply(data, 2, function(x) x / rep(x[1], length(x)))

    if(!is.na(datarange[[1]])) {
      datarange <- datarange[complete.cases(datarange), ]
      if(any(as.numeric(datarange[1,]) == 0))
        stop("xtsplot:  Can't normalize. First datarange row contains zeroes.")
      coredata(datarange) <- apply(datarange, 2, function(x) x / rep(x[1], length(x)))
    }

    sprint("  All curves normalized on: %s", index(data[1,]))
  }

  #------------------------------------------------------------
  # Tie recent curves to an equity curve
  #------------------------------------------------------------
  if(!is.na(tie_recent)) {


    # #############
    # library(xtsanalytics)
    # tie_recent = "VTI"
    # data  = xts_data[, 1:4]
    # #############

    # Remove leading rows containing only NAs
    nonas    <- apply(data, 1, function(x) !all(is.na(x)))
    data     <- data[nonas, ]

    # Identify starting date for each curve.
    nc               <- ncol(data)
    startdate        <- as.Date(rep("1980-01-01", nc))
    names(startdate) <- colnames(data)
    for(i in 1:nc)
      startdate[i] <- first(index(data[!is.na(data[, i]), ]))

    # Truncate leading NAs to set first date as first available data from tie_recent
    nonas    <- apply(data[, tie_recent], 1, function(x) !all(is.na(x)))
    data2    <- data[nonas, ]

    # Push startdates out to tie_recent if needed to ensure all startdates >= tie_recent
    for(i in 1:nc)
      if(startdate[i] <= startdate[tie_recent])
        startdate[i] <- startdate[tie_recent]

    # Normalize all equity curves into data2, keep leading NAs
    coredata(data2) <- apply(data2, 2, function(x) x / rep(first(na.omit(x)), length(x)))

    # Figure out the starting values for each curve from tie_recent
    startval        <- vector(mode = "numeric", length = nc)
    names(startval) <- colnames(data2)
    for(i in 1:nc)
      startval[i] <- data2[startdate[i], tie_recent]


    #----------------------------------------------------------
    # Multiply each equity curve by their associated startval
    # to tie their starting points on tie_recent
    #----------------------------------------------------------
    for(i in 1:nc)
      data2[, i] <- data2[, i] * startval[i]


    data <- data2

  }   ####### ENDIF tie_recent block  #########

  #------------------------------------------------------------
  # Adjust variables based on mode selected
  #------------------------------------------------------------
  switch(mode,
         portfolio = {
           # Default is portfolio, set up proper y labels
           if(is.na(ylab)) {
             if(norm) ylab <- "Norm. Prices" else
               ylab <- "Prices"
             }
           },
         gain = {
           # Normalize to 0% and turn off log scale
           sprint("gain mode selected.  Log scale is disabled.")
           if(is.na(ylab)) ylab <- "Gain (%)"
           data <- (data - 1) * 100
           log  <- ""

           },
         growthof100 = {
           # Normalize to 100
           sprint("growthof100 mode selected.")
           if(is.na(ylab)) ylab <- "Growth of $100"
           data <- data * 100

           },
         {
           # Default in switch
           stop("ERROR:  mode does not have a valid value.")
           })

  #------------------------------------------------------------
  # Set up common variables and put benchmark last
  #------------------------------------------------------------
  N       <- ncol(data)
  cn      <- colnames(data)
  columns <- 1:N

  lty     <- rep(lty, ceiling(N / length(lty)))[1:N]  # recycle line type
  lwd     <- rep(lwd, ceiling(N / length(lwd)))[1:N]  # recycle line width

  if(col[1] == "auto") {
    color_vec <- make_colors(n = N, type="lines")
    col       <- names(color_vec)
  } else {
    col       <- rep(col, ceiling(N/ length(col)))[1:N]   # recycle colors
    color_vec <- make_colors(n = 0, type="lines")
  }

  if(!all(col %in% names(color_vec))) stop("Invalid color name specified in argument col. Try ?make_colors.")
  col <- color_vec[col]

  # Select and move benchmark to last column to plot on top of others.
  if(length(bench) != 1) stop("Only one benchmark column allowed.")
  if(is.character(bench)) bench_i <- which(cn == bench) else bench_i <- bench
  if(bench_i != 0 && length(bench_i) == 1) {   # Not 0 nor integer(0)
    columns <- c(columns[-bench_i], bench_i)   # Move benchmark to last col.
    sprint("  Benchmark selected: %s", cn[bench_i])
    lwd[N]  <- 3            # Benchmark line width
    lty[N]  <- 1            # Benchmark line type
    col[N]  <- "#000000FF"  # Benchmark line is black
  } else {
    sprint("  No Benchmark selected.")
  }

  #------------------------------------------------
  # Select plotting method
  #------------------------------------------------
  method <- method[1]
  switch(method,
         zoo = {
           #---------------------------------------
           # zoo plot method
           #---------------------------------------
           sprint("  zoo plot method selected.")

           # Set up the plot area
           data <- data[, columns, drop=FALSE]

           if(is.na(ylab)) ylab <- "Prices"

           if("xts" %in% class(data))
             timeframe <- paste("Timeframe: ", index(data[1,]), "/",
                                 index(data[nrow(data), ])) else
               timeframe <- ""

           if(is.na(xlab)) xlab <- timeframe

           #sprint("timeframe is: %s", timeframe)

           # Plot the empty canvas region
           if(is.na(datarange[[1]])) datarange <- data
           zoo::plot.zoo(datarange, plot.type = "single", type="n",
                         log = log, xlab = xlab, ylab = ylab, col = col,
                         lwd = lwd, lty = lty, cex.lab = cex.lab, mgp = mgp, ...)

           # Plot shaded regions if specified
           if(!is.null(shaded)) draw_shaded_regions(shaded, shaded_col,
                                                    shaded_inv, log=log)

           # Add the curves to plot region
           for(i in 1:ncol(data)) {
             lines(as.zoo(data[, i]), col=col[i], lwd=lwd[i], lty=lty[i])
           }

           # Draw a vertical line if specified
           if(!is.na(vline[[1]])) {
             if(length(vline) >= 2) vlcol <- color_vec[vline[[2]]] else
               vlcol <- color_vec[1]
             if(length(vline) == 3) vlwd  <- vline[[3]] else vlwd = 1

             if("xts" %in% class(data)) vl <- as.Date(vline[[1]]) else
               vl <- vline[[1]]

             u <- par("usr")
             xvl <- c(vl, vl)
             if(log == "y") yvl <- c(10^u[3], 10^u[4]) else
               yvl <- c(u[3], u[4])
             lines(xvl, yvl, lty = "longdash", lwd = vlwd, col = vlcol)

             # Add a vertical label next to the vline, if specified
             if(!is.na(vlabel[[1]])) {
               text(x = as.numeric(as.Date(vl)), y = vlabel[[1]], labels = vlabel[[2]], srt = 90,
                    pos = 2, cex = cex.legend)

             }
           }

           # Draw a horizontal line if specified
           if(!is.na(hline[[1]])) {
             if(length(hline) == 2) hlcol <- color_vec[hline[[2]]] else
               hlcol <- color_vec[1]
             #if("xts" %in% class(data)) vl <- as.Date(vline[[1]]) else
             #  vl <- vline[[1]]
             hl  <- hline[[1]]

             #abline(h = hl, lty = "dotted", lwd = 1, col = hlcol)

             u   <- par("usr")
             xhl <- c(u[1], u[2])
             yhl <- c(hl, hl)
             lines(xhl, yhl, lty = "dotted", lwd = 1, col = hlcol)
           }

           # Add the legend if specified
           if(legend != "none") {
             legnames <- colnames(data)
             ndata    <- length(legnames)
             legcol   <- col
             leglwd   <- lwd

             # If benchmark exists & multiple curves, then move bench to 1st item on the legend
             if(bench !=0 && ndata > 1) {
               legnames <- c(legnames[ndata], legnames[1:(ndata-1)])
               legcol   <- c(col[ndata], col[1:(ndata-1)])
               leglwd   <- c(lwd[ndata], lwd[1:(ndata-1)])
             }

             legend(legend, legend = legnames, col = legcol,
                    lwd=leglwd, pch=19, cex=cex.legend, bg="grey97")
           }

           #--------------------------------------------------
           #  Print the plot to file if a file name is given
           #--------------------------------------------------
           if(!is.null(fname)) {

             fullname <- fnamestamp(paste0(fname, ".png"))
             png(file=fullname, bg='grey99', width=pngsize[1], height=pngsize[2])

             par(mar = c(5.2, 6, 5, 2.2))
             zoo::plot.zoo(data, plot.type = "single",
                           log = log, xlab = "Time", ylab = "Prices",
                           col = col, lwd = lwd, lty = lty,
                           cex=2, cex.axis = 2, cex.lab = 2, cex.main=3,
                           fg = "grey10", ...)
             if(legend != "none") {
               legend(legend, legend = colnames(data), col = col,
                      lwd=lwd+1, pch=19, cex=1.7, bg="grey97")
             }
             dev.off()
             sprint("  Plot also sent to file: %s", fname)

           }

         }, ####  END zoo method  ####
         xts = {
           #---------------------------------------
           # xts plot method
           #---------------------------------------
           sprint("  xts method selected (not yet implemented).")


         }, ####  END xts plot method  ####
         custom = {
           #---------------------------------------
           # custom plot method
           #---------------------------------------
           sprint("  Custom method not yet implemented.  Nothing plotted.")

         }, ####  END custom plot method  ####
         {
           #---------------------------------------
           # Wrong Method Selected, so Stop!
           #---------------------------------------
           stop("  Improper plot method selected.")
         }
         )

  #------------------------------------------------
  #  Restore default parameters before exiting
  #------------------------------------------------
  #par(save_par)

  if(return_xts) return(data)

}   ######  END FUNCTION xtsplot  ######


#-------------------------------------------------------------------------------------
#  FUNCTION draw_shaded_regions
#
#  Internal function - not exported
#-------------------------------------------------------------------------------------
draw_shaded_regions <- function(shaded, shaded_col, shaded_inv, log='') {

  u <- par("usr")
  N <- ncol(shaded)

  # convert u to 10^u if log in use
  if(log == "y") u[3:4] <- 10^u[3:4]

  # recycle shaded_inv and shaded_col as necessary
  shaded_inv  <- rep(shaded_inv, ceiling(N / length(shaded_inv)))[1:N]
  shaded_col  <- rep(shaded_col, ceiling(N / length(shaded_col)))[1:N]

  region_cols <- make_colors(n = 0, type = 'regions')

  # Inside the for k loop
  for(k in 1:N) {
    regions <- get_shaded_regions(shaded[, k], shaded_inv[k])

    sprint("Shaded region in %s for: %s", shaded_col[k], colnames(shaded)[k])
    # Loop over each region and draw the rectangle
    nreg <- nrow(regions)
    if(nreg > 0) {
      for(i in 1:nrow(regions)) {

        rect(regions[i, 1], u[3], regions[i, 2], u[4],
             border = NA, col = as.character(region_cols[shaded_col[k]]))
      }
    }

  }


  #  Outside the for k loop:  redraw the lines.
  abline(h=u[3])   # Redraw bottom line for plot box outline
  abline(h=u[4])   # Redraw top line for plot box outline


}  ##### END FUNCTION draw_shaded_regions #####


#-------------------------------------------------------------------------------------
#  FUNCTION make_colors
#'
#' Make a nice color palette appropriate for plotting functions
#'
#' @param n           Number of colors to return.  If n is larger than the palette
#'                    selected, then the colors are recycled.
#'
#' @param type        The type of color palette to create.  If type = 'lines'
#'                    (the default), then the palette is a set of colors most
#'                    appropriate to plot lines on a white background.
#'                    If type = 'regions' then the palette is a set of
#'                    pastel colors appropriate to highlight regions on
#'                    a plot or to make a bar graph (with a preset level of alpha).
#'
#' @param showcolors  Logical.  Specifies whether to plot a bar chart that shows
#'                    the colors in the selected palette with their names.
#'
#' @param alpha       The colors alpha or transparency for the entire set.
#'                    Defined as a 2 character hex string to be appended to
#'                    each color.  Ignored when type = "regions". Default is "FF"
#'                    for no transparency.
#'
#' @return Returns a named vector of RGB colors (in hex format).
#'         xtsanalytics functions can refer to the names of these colors rather
#'         than their index number when the user specifies a given set of colors.
#'         See xtsplot for details.
#'
#' @export
#-------------------------------------------------------------------------------------
make_colors <- function(n = 0, type = c('lines', 'regions'),
                        showcolors = FALSE, alpha = "FF" ) {

#
#   col         = c('black', 'blue', 'green', 'red', 'orange',
#                   'purple', 'brown', 'darkpink', 'grey', 'turquoise',
#                   'mauve', 'lightblue', 'lightgreen', 'pink',
#                   'lightorange', 'lightpurple', 'yellow')
#
#   # Recycle colors if necessary
#   if(n != 0) col <- rep(col, ceiling(n/ length(col)))[1:n]

  # Function to convert color names to hex RGB value
  color2hex <- function(color_name) {
    x        <- as.numeric(col2rgb(color_name))
    hexcolor <- substr(rgb(x[1], x[2], x[3], 1, maxColorValue = 255), 1, 7)
    return(hexcolor)
  }

  switch(type[1],
         #----------------------------------------------
         #  lines color type selection
         #----------------------------------------------
         lines = {
           greycol    <- color2hex("grey60")    # Get hex RGB color value for grey
           blackcol   <- color2hex("black")     # Get hex RGB color value for black

           # 17 colors
           col        <- c(blackcol, brewer.pal(12, "Paired")[-11],
                           brewer.pal(12, "Set3")[c(-2, -3, -5, -6, -7, -8, -9, -11)], greycol)
           names(col) <- c('black', 'lightblue', 'blue', 'lightgreen', 'green', 'pink',
                           'red', 'lightorange', 'orange', 'lightpurple', 'purple',
                           'brown', 'turquoise', 'darkpink', 'mauve', 'yellow', 'grey')

           # Reorder the colors
           col        <- col[c(3,5,7,9,11,12,14,16,1,2,4,6,8,10,13,15,17)]
         },
         #----------------------------------------------
         #  regions color type selection
         #----------------------------------------------
         regions = {
           col        <- c(brewer.pal(9, "Pastel1")[-9], color2hex("grey80"))
           names(col) <- c('red', 'blue', 'green', 'purple', 'orange',
                           'yellow', 'brown', 'pink', 'grey')

           # Now add alpha (transparency) by appending hex value
           col[] <- paste0(col, "7F")
           #print(col)
         }, {
           stop("Invalid color group type selected.")

         })  #####  END switch on type  #####


  # Show a barchart of available colors if specified
  if(showcolors) {
    N <- length(col)
    x <- rep(1, N)

    save_par <- par()
    par(mar=c(5,7,4,2))
    barplot(x, col=col, names.arg = names(col), horiz = TRUE, las=1,
            main = paste(type[1], "color reference chart"))

    par(mar = save_par$mar)

  }

  # Recycle color palette if necessary, truncate return value
  if(n == 0) n <- length(col)
  col2 <- rep(col, ceiling(n / length(col)))[1:n]

  # Append the alpha to the palette unless type == "regions"
  if(type[[1]] != "regions")
    col3 <- paste0(col2, alpha)
  else
    col3 <- col2

  names(col3) <- names(col2)

  return(col3)


}   ####  END FUNCTION make_colors  ####




#----------------------------------------------------------------------
#  FUNCTION get_shaded_regions
#
#  Internal function - NOT exported.
#
#  data:    A one column xts (drop=FALSE) with a logical value (0 or 1)
#           specifying whether a box is drawn.  Normally, this would
#           be a timer.
#
#  invert:  Logical.  Specifies whether data should be inverted.  A
#           market timer is normally inverted since bear markets are
#           zeroes and this is when we typically want the regions
#           highlighted.
#
#  Returns a dataframe of 2 columns:  the start date and end date of
#  the regions to draw a box around.
#----------------------------------------------------------------------
get_shaded_regions <- function(data, invert=TRUE) {

  if(ncol(data) != 1) stop("Only one set of shaded regions allowed.")
  colnames(data) <- 'box_logi'

  # invert data if specified:  1 = draw box, 0 = no box
  if(invert) data$box_logi  <- !data$box_logi     # invert the data

  data <- data[complete.cases(data), , drop=FALSE]
  N    <- nrow(data)
  data$transitions <- c(NA, as.numeric(data[2:N, 1]) - as.numeric(data[1:(N-1), 1]))

  # box_logi == 1  >> box is drawn
  # First date box setup:  is there a box at time zero?
  if(data$box_logi[1] == 1) data$transitions[1] <- 1 else
    data$transitions[1]  <- 0

  # build shaded box X-coordinates
  xbox_start <- zoo::index(data[data$transitions ==  1, ])
  xbox_end   <- zoo::index(data[data$transitions == -1, ])

  # check if box end date is missing (currently a bear)
  if(length(xbox_end) == length(xbox_start) - 1) {
    xbox_end <- c(xbox_end, index(data[N]))
  }

  df <- data.frame(box_start = xbox_start, box_end=xbox_end)

  return(df)


}  #####  END FUNCTION get_shaded_regions  #####


  ###########################

