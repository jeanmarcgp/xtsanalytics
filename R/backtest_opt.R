#
#
# FUNCTION backtest_opt.R
#
#' Function to backtest a periodically optimized portfolio (empty)
#'
#' This function repeatedly calls function optimize.portfolio in PortfolioAnalytics
#' to perform a backtest of a strategy requiring optimization. Since optimize.portfolio
#' can be slow to run, it provides periodic updates at each optimization date via a log
#' file. It is assumed to run on multiple CPU cores using foreach and the snow package.
#'
#' This function uses the version 2 of optimize.portfolio.  More details by
#' ?optimize.portfolio.
#'
#'@param R  the returns
#'
#'@return return value
#'@examples
#'  backtest_opt()
#'@export
backtest_opt <- function(R, portfolio, optimize_method = "random",
                         search_size = 100                         ) {

}
