
####################################################################################
# FILE wfo_summary.R
#
#
#
####################################################################################
# FUNCTION wfo_summary
#
#' Create a summary of key statistics from a Walk-Forward Optimization (WFO) run
#'
#'
#' @param  wfodata    The data structure containing the walk-forward optimization
#'                    information.  It should be of the proper class to run the
#'                    right underlying code generatic the summary.
#'
#' @return Returns an appropriate data structure summarizing the WFO run under
#'         analysis.  This normally includes an equity curve and, if a statfolio
#'         was provided, it would also include additional times series that quantifies
#'         the statistical properties of the set of equity curves resulting from the
#'         statfolio (details TBD).
#'
#' @seealso wfo_statfolio
#'
#' @export
#----------------------------------------------------------------------------------
wfo_summary <- function(wfodata) {

  valid_classes <- c("optimize.portfolio.rebalancing")
  wfoclass      <- class(wfodata)

  if(!any(wfoclass %in% valid_classes)) stop("wfo_summary: Object not of a valid class")

  switch(wfoclass,
         optimize.portfolio.rebalancing = {
           #-------------------------------------------------------------------
           # Leverage the existing summary method, but fix the returns
           #-------------------------------------------------------------------
           wfosumm  <- summary(wfodata, geometric = FALSE)  # doesn't work: always returns geometric

           # Replace out.summ$portfolio_returns with the right values!
           x        <- Return.rebalancing(returns, wfosumm$weights,
                                          verbose = TRUE, geometric = FALSE)
           portrets                  <- x$returns
           colnames(portrets)        <- "Portfolio"
           wfosumm$portfolio_returns <- portrets

         }, {
           # Default if none are true
           stop("wfo_summary:  Object not of a valid class")
         })


  return(wfosumm)

}  ##########  END FUNCTION wfo_summary  ##########

