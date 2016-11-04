#
#
#  Devel.R
#
#  Script to exercise functions during their initial development.  Some
#  of the tests may end up in the functions respective testthat files.
#

# base script to test random_portfolios
library(xtsanalytics)
library(microbenchmark)

symbols <- c('XLB', 'XLE', 'XLF', 'XLI', 'XLK', 'XLP', 'XLU', 'XLV', 'XLY',
             'MDY', 'EWJ', 'EWG', 'EWH', 'EWU', 'EWW', 'EWC', 'EWA', 'EWP',
             'EWL', 'EWI', 'EWS', 'EWM', 'EWD', 'EWQ', 'EWN', 'EWK', 'EWO',
             'SPY')

prices   <- mget_symbols(symbols, from = "1999-01-01")
prices   <- prices[complete.cases(prices), ]
pricesm  <- to.monthly(prices, OHLC = FALSE)
data     <- TTR::ROC(pricesm, type = "discrete")

# microbenchmarks
N        <- 1000        # number of portfolios
K        <- 2          # number of assets per random portfolio
weights  <- "uniform"  # random uniform distribution of asset weights (normalized to sum = 1)
sprint("\nNumber of assets: %s", ncol(data))
m1 <- microbenchmark(random_portfolios(data, N, K, weights),
                     random_portfolios(data[, 1:4], N, K, weights),
                     times = 25)
print(m1)


weights  <- "equal"    # Equal asset weights (normalized to sum = 1)
sprint("\nNumber of assets: %s", ncol(data))
m2 <- microbenchmark(random_portfolios(data, N, K, weights),
                     random_portfolios(data[, 1:4], N, K, weights),
                     times = 25)
print(m2)






N        <- 500
K        <- 2
weights  <- "uniform"

rs     <- random_portfolios(data, N = N, K = K, weights = weights, return_ec = TRUE)
xtsplot(rs$ec, bench = "ecavg", main = "all equity curves")



##########################################################################\


stop("DEVEL.R End")

