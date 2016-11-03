
####################################################################################
# FILE plot_statfolio.R
#
#
# FUNCTIONS TO DO:
# . need a function to subset wfodata, to chart weights for a period subset
# . need a low level function underneath subfolio_curve, to just compute
#   an equity curve from a set of fixed weight, given a rebalancing set of
#   dates.  This is to replace Returns.rebalancing.
# . Also, the random portfolio function could be written using the subfolio and
#   stitch_statfolio functions...  Basically, create some fake wfodata that
#   includes equal weights (or 100 random weights?), then stitch 1000 curves and
#   return the average curve.  This may be faster and likely closer to reality with
#   monthly rebalancing...
#     -> Instead, custom write it.  rebalance monthly, randomly pick N Assets equal
#        weights, and build 1000 curves.
#
#
####################################################################################
# FUNCTION plot_statfolio
#'
#' Plot equity curves and sensitivity information related to a statfolio
#'
#' @param statfolio   A statfolio of class "optimize.portfolio.rebalancing" as
#'                    created by a WFO run using function wfo_statfolio.
#'
#' @param tf          The statfolio timeframe to consider, using standard xts
#'                    rules and formatting.  If tf = "all" (default) then the
#'                    entire available timeframe is used.
#'
#' @param type        The type of plot to generate.  Available types include
#'                    "SDweights" (default), which generates three equity curves
#'                    (best, average and worst) based on the weight variations
#'                    resulting from the WFO runs on the top panel.  On the bottom
#'                    panel, a time plots of the standard deviation of each asset
#'                    weight is shown.  This helps visualize whether the optimizer
#'                    converges to a stable set of weights (low SD) and whether that
#'                    variation, not matter how small, results in meaningful changes in
#'                    portfolio performance over time.
#'
#' @return Nothing is returned, except that a plot is generated on the graphic device.
#'
#' @export
#-----------------------------------------------------------------------------------
plot_statfolio <- function(statfolio, tf = "all", type = "SDweights", Ncurves = 100,
                           main = "Statfolio Performance Summary", log = "y", ...) {

  # #######################
  # ####  Devel code  #####
  # #######################
  # library(xtsanalytics)
  # load("./ignore-results/wfodata.Rdata")
  # statfolio = wfodata
  # tf        = "2011-10/2013-03"
  # Ncurves   = 100
  # main      = "Statfolio Performance Summary"
  # log       = "y"
  # ############################

  # Subset the statfolio and compute equity curves
  newfolio <- subset_statfolio(statfolio, tf = tf)
  ec       <- stitch_statfolio(newfolio, Ncurves = Ncurves)[, 1:3]

  # Extract the weights SD and create an xts

  # Set up the plot regions and plot charts
  op    <- par(no.readonly = TRUE)
  layout(matrix(c(1, 2)), heights = c(2, 1.5), widths = 1)
  par(mar = c(3, 5, 4, 2))

  cols  <- c("green", "red")
  xtsplot(ec, col = cols, main = main, log = log, bench = "Average_Weights", cex.main = 1.5)


  sdweights <- extract_SDweights(newfolio, tf = tf)
  xtsplot(sdweights, main = "Standard Deviation of Weights Between Optimization Runs", norm = FALSE)


  par(op)


}

