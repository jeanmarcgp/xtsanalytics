
####################################################################################
# FILE optimize_statfolio.R
#
#
# To Do - functions to write
# . extract_statfolio to get the returns xts from the wfo data structure
#   -> make sure this tests cleanly so write a testthat to exercise the function
# . plot_page:  this is a generic function to plot several lines of text to a pdf page
#   This should be a wrapper using textplot()
#
####################################################################################
# FUNCTION optimize_statfolio
#
#' Optimize a portfolio of assets N times to get asset weight statistics
#'
#' Optimze a portfolio of assets N times in order to gather statistics
#' on the portfolio asset weights.  The optimization is performed at the
#' most recent date as provided by the xts matrix of returns.
#'
#' This function leverages the optimize.portfolio function from package
#' PortfolioAnalytics. It differs from optimize.portfolio in
#' several ways. First, an argument N is provided to repeatedly call
#' optimize.portfolio in order to gather statistics on the asset weights.
#' This is relevant because often, the optimization of the objective function
#' results in a somewhat unstable optimum, resulting in many possible asset
#' weights.
#'
#' See the vignette xtsanalytics for more details on the statfolio data structure.
#'
#'
#' @param rets             An xts matrix of asset returns.
#'
#' @param portfolio        An object of type "portfolio" specifying the constraints
#'                         and objective function.
#'
#' @param train_window     The training window (in days) used to subset rets to calculate
#'                         the optimization matrix. This is normally the same as the
#'                         rolling_window if WFO optimization is used.
#'
#' @param N                The number of times to repeatedly call function
#'                         optimize.portfolio to generate weight statistics.
#'                         Default is 1.
#'
#' @param weightFUN        Sets the method used to compute the asset weights returned
#'                         from all runs of optimize.portfolio.  Default is "mean",
#'                         but it can be any valid function name such as "median" or other.
#'
#' @param objfnmat         The feature matrix passed to the custom objective function, if
#'                         specified, used by optimize.portfolio.
#'
#' @param maxwtsmat        The maximum weights xts matrix.  It should include a row with
#'                         the current optimization date, which will be extracted as a vector
#'                         and added as a constraint in the optimize.portfolio function call.
#'                         Ignored if not specified.
#'
#' @param optimize_method  Sets the optimization method used by optimize.portfolio.
#'                         Default is "DEoptim".
#'
#' @param ...              Additional arguments passed through to optimize.portfolio.
#'
#'
#' @return  Returns a list containing the same elements as what optimize.portfolio
#'          normally returns, with the following exceptions:
#'
#' \describe{
#'   \item{\preformatted{$weights}}{
#'      A named vector containing the optimal set of weights for the portfolio.
#'      If N > 1, then this is a statistic computed from all optimize.portfolio
#'      runs.  The choice of statistic is a function named using argument weightFUN.
#'      Default is "mean".
#'   }
#'   \item{\preformatted{$N}}{
#'      The number of times function optimize.portfolio was called.
#'   }
#'   \item{\preformatted{$allweights}}{
#'      A matrix containing the optimized asset weights for each run of
#'      optimize.portfolio
#'   }
#'   \item{\preformatted{$SD_weights}}{
#'      A named vector containing the standard deviations of the weight values,
#'      computed by taking the StdDev of $allweights.  If N = 1, then this
#'      will be all zeroes.
#'   }
#' }
#'
#' @seealso optimize.portfolio
#'
#' @export
#-----------------------------------------------------------------------------------
optimize_statfolio <- function(rets, portfolio, train_window = 63, N = 1,
                               weightFUN = "mean", objfnmat = NA, maxwtsmat = NA,
                               optimize_method = "DEoptim", ...) {

  #----------------------------------------------------------
  # Optimize at the most recent date
  #----------------------------------------------------------
  lastN            <- nrow(rets)
  most_recent_rets <- rets[(lastN - train_window):lastN, ]

  out <- list()

  sprint("Running optimization on: %s", index(last(rets)))
  sprint("  ===> Repeating optimization %s times...", N)
  #if(is.na(objfnmat[[1]])) stop("optimize_statfolio:  objfnmat is NA.  Stopping now.")                         )
  print("tail(objfnmat):")
  print(tail(objfnmat))

  wts_mat           <- matrix(nrow = N, ncol = ncol(rets))
  colnames(wts_mat) <- colnames(rets)

  # Extract the max weights vector from maxwtsmat
  maxweights <- maxwtsmat[index(last(rets)), ]
  # print(str(maxweights))
  # sprint("maxwtsmat:")
  # print(str(maxwtsmat))
  # print(index(last(rets)))
  # print(tail(maxwtsmat))

  maxwtsvec  <- as.vector(maxweights)
  names(maxwtsvec) <- colnames(maxweights)

  temp_port <- portfolio
  temp_port <- add.constraint(temp_port, type = "box", min = 0.0, max = maxwtsvec)
  print(maxwtsvec)
 # print(temp_port)

  for(i in 1:N) {
    mrp_opt <- optimize.portfolio(R = most_recent_rets, portfolio = temp_port,
                                  optimize_method = optimize_method,
                                  trace = TRUE, ...)
    out[[i]]      <- mrp_opt
    wts_mat[i, ]  <- mrp_opt$weights
  }

  #------------------------------------------------------------
  # Compute run statistics to report back
  #------------------------------------------------------------
  wts_avg <- apply(wts_mat, 2, weightFUN)
  wts_SD  <- apply(wts_mat, 2, stats::sd)

  #------------------------------------------------------------
  # Build statfolio data structure based on last run
  # and add relevant additional data and statistics
  #------------------------------------------------------------
  outlist             <- mrp_opt
  outlist$weights     <- wts_avg    # replace with average weights
  outlist$N           <- N
  outlist$allweights  <- wts_mat
  outlist$SD_weights  <- wts_SD

  return(outlist)


}



