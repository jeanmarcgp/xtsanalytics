
####################################################################################
# FILE wfo_statfolio.R
#
#
#
####################################################################################
# FUNCTION wfo_statfolio
#
#' Optimizes a portfolio of assets using walk-forward optimization
#'
#' This function repeated calls PortfolioAnalytics::optimize.portfolio at
#' a set of pre-defined dates to achieve walk-forward optimization. It is
#' similar to PortfolioAnalytics::optimize.portfolio.rebalancing except that
#' it allows for custom objective functions and it is possible to modify the
#' asset universe at each WFO date dynamically. This is useful when, for example,
#' the asset universe is screened for certain criteria prior to optimizing the
#' portfolio.
#'
#' @param rets             An xts matrix of asset returns.
#'
#' @param portfolio        An object of type "portfolio" specifying the constraints
#'                         and objective function.
#'
#' @param roll_window      The rolling window (in days) used to subset rets to calculate
#'                         the optimization matrix.
#'
#' @param N                The number of times to repeatedly call function
#'                         optimize.portfolio to generate weight statistics at
#'                         each wfo_span points in time.  Default is 1.
#'
#' @param Ncores           The number of CPU cores used to run the WFO optimization in
#'                         parallel.  When set to 1 (the default), the code is single-threaded.
#'                         This does not automatically means only one core is used as some
#'                         underlying R functions may be multi-threaded by design.
#'                         When Ncores > 1, then the code will spawn a parallel backend and
#'                         use the foreach package to run its parallel jobs.  Foreach prevents
#'                         any outputs to the console, so the only way to see progress via
#'                         the console is to set Ncores = 1.
#'
#' @param wfo_span         The frequency (dates) when the portfolio is reoptimized and
#'                         rebalanced.  This can be set to any of: "weeks", "months", "quarters"
#'                         or "years".  Can be overridden with specific dates if such dates
#'                         are provided as a character vector of valid dates.
#'
#' @param weightFUN        Sets the method used to compute the asset weights returned
#'                         from all runs of optimize.portfolio.  Default is "mean",
#'                         but it can be any valid function name such as "median" or other.
#'
#' @param objfnmat         The feature matrix passed to the custom objective function, if
#'                         specified, used by optimize.portfolio.
#'
#' @param maxwtsmat        The maximum weights xts matrix, indexed by the WFO dates
#'                         so that maximum weights at these dates can be extracted before
#'                         calling optimize_statfolio.  The max weights are specified as
#'                         an added constraint in the optimize.portfolio function call.
#'                         Ignored if not specified.
#'
#' @param optimize_method  Sets the optimization method used by optimize.portfolio.
#'                         Default is "DEoptim".
#'
#' @param ...              Additional arguments passed through to optimize.portfolio.
#'
#'
#' @export
#-----------------------------------------------------------------------------------
wfo_statfolio <- function(rets, portfolio, roll_window = 63, N = 1, Ncores = 1,
                          wfo_span = "months", weightFUN = "mean", objfnmat = NA,
                          maxwtsmat = NA, optimize_method = "DEoptim", ...) {


  if(Ncores == 1) {
    #----------------------------------------------------------------
    # Explicit WFO code, single threaded.
    #----------------------------------------------------------------
    sprint("Now running the single-threaded WFO code")
    start_t <- Sys.time()

    ep_raw  <- endpoints(rets, on = wfo_span)  # all endpoints
    ep_i    <- ep_raw[which(ep_raw >= roll_window)]  # only those > training period.

    # Loop through each endpoint optimization, single-threaded
    outlist    <- list()   # All WFO dates (superset)
    for(k in 1:length(ep_i)) {
      sprint("\nWFO Optimizing Date: %s...", index(rets[ep_i[k]]))

      #------------------------------------------------------------
      # Subset returns as needed by rolling window
      #------------------------------------------------------------
      win_start     <- ep_i[k] - roll_window + 1
      win_end       <- ep_i[k]
      rets_window   <- rets[win_start:win_end]
      sprint("Optimization window starts at: %s", index(rets_window[1, ]))
      sprint("Optimization window ends at:   %s", index(last(rets_window)))

      folio <- optimize_statfolio(rets            = rets_window,
                                  portfolio       = portfolio,
                                  train_window    = roll_window,
                                  N               = N,
                                  weightFUN       = weightFUN,
                                  objfnmat        = objfnmat,
                                  maxwtsmat       = maxwtsmat,
                                  optimize_method = optimize_method,
                                  ...             = ...)

      outlist[[k]]   <- folio

    }  #### end for k loop ####

    names(outlist) <- index(returns[ep_i])

  } else {
    #----------------------------------------------------------------
    # Multi-CPU job, spawn parallel backend
    #----------------------------------------------------------------
    sprint(">>>>>  Spawning parallel jobs for running WFO optimization")
    start_t <- Sys.time()

    ep_raw  <- endpoints(rets, on = wfo_span)  # all endpoints
    ep_i    <- ep_raw[which(ep_raw >= roll_window)]  # only those > training period.

    outlist <- list()   # All WFO dates (superset)
    ndates  <- length(ep_i)
    sprint("Number of dates to optimize: %s", ndates)

    #-----------------------------------------------------------
    # Start multi-core jobs, store results in outlist
    #-----------------------------------------------------------
    cl <- start_parallel(Ncores = Ncores)
    outlist <- foreach(k = 1:ndates, .packages = "xtsanalytics") %dopar% {
                         #------------------------------------------------------------
                         # Subset returns as needed by rolling window
                         #------------------------------------------------------------
                         win_start     <- ep_i[k] - roll_window + 1
                         win_end       <- ep_i[k]
                         rets_window   <- rets[win_start:win_end]

                         folio <- optimize_statfolio(rets            = rets_window,
                                                     portfolio       = portfolio,
                                                     train_window    = roll_window,
                                                     N               = N,
                                                     weightFUN       = weightFUN,
                                                     objfnmat        = objfnmat,
                                                     maxwtsmat       = maxwtsmat,
                                                     optimize_method = optimize_method,
                                                     ...             = ...)


                         }  ######  END foreach multicore  #####
    #----------------------------------
    # Stop parallel cluster
    #----------------------------------
    stop_parallel(cl)

    print(index(returns[ep_i]))
    sprint("length of WFO outlist: %s", length(outlist))
    sprint("Last folio results:")
    print(outlist[[length(outlist)]])

    # Name each outlist folio by its WFO date
    names(outlist) <- index(returns[ep_i])

  }  ########## END else - parallel multicore execution ##########

  #------------------------------------------------
  # Build the statfolio and return the results
  #------------------------------------------------
  end_t      <- Sys.time()
  elapsed_t  <- end_t - start_t
  wfo_out <- build_statfolio(outlist, rets, elapsed_t)

  return(wfo_out)


}   #########  END wfo_statfolio  #########




#--------------------------------------------------
# HELPER FUNCTION - NOT EXPORTED
#
# Build a data structure similar to
# optimize.portfolio.rebalancing structure while
# also adding the statfolio information.
# The returns provided should be the entire xts
# used for the wfo optimization.
#--------------------------------------------------
build_statfolio <- function(reslist, returns, elapsed_time) {

  out                 <- list()
  out$portfolio       <- reslist[[1]]$portfolio
  out$R               <- returns
  out$call            <- reslist[[1]]$call
  out$elapsed_time    <- elapsed_time
  out$opt_rebalancing <- reslist

  class(out)          <- c("optimize.portfolio.rebalancing")

  return(out)

}

