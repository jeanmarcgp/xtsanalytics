#
#
#  Devel.R
#
#  Script to exercise functions during their initial development.  Some
#  of the tests may end up in the functions respective testthat files.
#

library(xtsanalytics)

#---------------------------------------------------------------
# Run parameters
#---------------------------------------------------------------
mpar <- list(src         = "database",     # Where to find prices
             maxbox      = 0.7,            # Maximum asset weight in portfolio (before vol adjustment)
             objfcn      = "Momentum",     # objective function to maximize
             riskfcn     = "StdDev",       # objective function to simultaneously minimize
             timeframe   = "1997/2016",    # prices data timeframe
             #timeframe   = "2016/2016",
             normfactor  = 1.0,            # Momentum factor vs. risk during optimization
             mom_per     = 126,            # Momentum period to use for optimization
             Nrepeat     = 2,              # Number of times to repeat run at each WFO date
             roll_window = 126,           # Rolling window size for WFO risk (SD) optimization
             wfo_span    = "months",      # WFO optimization dates
             volfeature  = "sd63",        # volatility used to adjust maxbox for each asset
             volthresh   = 0.18,          # volatility threshold beyond which maxbox is adjusted down
             momfeature  = "mom126",      # momentum used to rank assets
             momthresh   = 0.0,           # absolute annual momentum level required to include an asset
             cashasset   = "VSGBX",
             rankthresh  = 5              # relative momentum rank to include an asset i.e. top 5
)

#---------------------------------------------------------------
# Generic Script Parameters
#---------------------------------------------------------------
asset_fname <- "../../Investing/Portfolio Optimization/Data/Asset Universe.xlsx"
pdfout      <- TRUE
Ncores      <- 7
load_data   <- FALSE
save_wfo    <- TRUE

#----------------------------------------------------------------
# Load Symbols list from Excel file
#----------------------------------------------------------------
starttime()

sprint("Loading symbols list from Excel file:  %s", asset_fname)
symdf    <- read_excel(asset_fname)
symbols  <- as.character(symdf[which(symdf[, "InModel"] == "Y"), "Symbol"])

#--------------------------------------------------
# Download the prices and align on endpoints
#--------------------------------------------------
prices   <- mget_symbols(symbols, src = mpar$src, filepath = "../../Investing/DATABASE/data")
Nsymbols <- length(symbols)
prices   <- prices[complete.cases(prices), ]


#---------------------------------------------------------
# Compute the momentum feature matrix
# Normalize momentum with standard deviations so the
# optimization puts similar weight on both!
#---------------------------------------------------------
sprint("Calculating the features matrix...")
feature  <- paste0("mom", mpar$mom_per)
featmat2 <- make_features(prices, features = feature)[[1]]
featmat  <- (1 + featmat2)^(252 / mpar$mom_per) - 1
featmat  <- featmat / sqrt(252) * mpar$normfactor


#---------------------------------------------------------
# Compute the max weights matrix
#---------------------------------------------------------
sprint("Calculating the maximum weights matrix...")
maxwts  <- maxscreen(prices, maxweights = mpar$maxbox,
                     volfeature         = mpar$volfeature,
                     volthresh          = mpar$volthresh,
                     momfeature         = mpar$momfeature,
                     momthresh          = mpar$momthresh,
                     cashasset          = mpar$cashasset,
                     rankthresh         = mpar$rankthresh )

#--------------------------------------------------
# Compute returns and align every xts on timeframe
#--------------------------------------------------

sprint("point 1")
returns  <- TTR::ROC(prices, type = "discrete")
returns  <- returns[complete.cases(returns), ]
returns  <- returns[mpar$timeframe, ]
sprint("point 2")
prices   <- prices[mpar$timeframe, ]
featmat  <- featmat[mpar$timeframe, ]
maxwts   <- maxwts[mpar$timeframe, ]


#----------------------------------------------------------
# Define baseline portfolio constraints
# NOTE:  Box constraints are passed to the function call
#----------------------------------------------------------
sprint("Setting up the portfolio...")
init.portfolio <- portfolio.spec(assets = colnames(returns))
init.portfolio <- add.constraint(portfolio = init.portfolio, type = "leverage",
                                 min_sum = 0.99, max_sum = 1.01)

#----------------------------------------------------------
# Set objectives with its custom arguments list
#  - risk is the riskfcn (to minimize)
#  - return is the objfcn (to maximize)
#----------------------------------------------------------
minvarport <- add.objective(portfolio = init.portfolio,
                            type = "risk", name = mpar$riskfcn)
optport    <- add.objective(portfolio = minvarport,
                            type = "return", name = mpar$objfcn,
                            arguments = list(objfnmat = featmat))


#------------------------------------------------------------
# Optimize at the most recent date
# Subset returns as needed by rolling window
#------------------------------------------------------------
set.seed(14)

most_recent_date <- index(returns[nrow(returns),])
most_recent_i    <- returns[most_recent_date, , which.i = TRUE]
win_start        <- most_recent_i - mpar$roll_window + 1
win_end          <- most_recent_i
rets_window      <- returns[win_start:win_end]
sprint("Most recent optimization window starts at: %s", index(rets_window[1, ]))
sprint("Most recent optimization window ends at:   %s", index(last(rets_window)))
mrd_opt <- optimize_statfolio(rets            = rets_window,
                              portfolio       = optport,
                              train_window    = mpar$roll_window,
                              N               = mpar$Nrepeat,
                              weightFUN       = "mean",
                              objfnmat        = featmat,
                              maxwtsmat       = maxwts,
                              optimize_method = "DEoptim",
                              traceDE         = 5 )

plot(mrd_opt, risk.col = mpar$riskfcn, return.col = mpar$objfcn,
     main = paste("Portfolio Optimization on", most_recent_date))











#
# # base script to test random_portfolios
# library(xtsanalytics)
# library(microbenchmark)
#
# symbols <- c('XLB', 'XLE', 'XLF', 'XLI', 'XLK', 'XLP', 'XLU', 'XLV', 'XLY',
#              'MDY', 'EWJ', 'EWG', 'EWH', 'EWU', 'EWW', 'EWC', 'EWA', 'EWP',
#              'EWL', 'EWI', 'EWS', 'EWM', 'EWD', 'EWQ', 'EWN', 'EWK', 'EWO',
#              'SPY')
#
# prices   <- mget_symbols(symbols, from = "1999-01-01")
# prices   <- prices[complete.cases(prices), ]
# pricesm  <- to.monthly(prices, OHLC = FALSE)
# data     <- TTR::ROC(pricesm, type = "discrete")
#
# # microbenchmarks
# N        <- 1000        # number of portfolios
# K        <- 2          # number of assets per random portfolio
# weights  <- "uniform"  # random uniform distribution of asset weights (normalized to sum = 1)
# sprint("\nNumber of assets: %s", ncol(data))
# m1 <- microbenchmark(random_portfolios(data, N, K, weights),
#                      random_portfolios(data[, 1:4], N, K, weights),
#                      times = 25)
# print(m1)
#
#
# weights  <- "equal"    # Equal asset weights (normalized to sum = 1)
# sprint("\nNumber of assets: %s", ncol(data))
# m2 <- microbenchmark(random_portfolios(data, N, K, weights),
#                      random_portfolios(data[, 1:4], N, K, weights),
#                      times = 25)
# print(m2)
#
#
#
#
#
#
# N        <- 500
# K        <- 2
# weights  <- "uniform"
#
# rs     <- random_portfolios(data, N = N, K = K, weights = weights, return_ec = TRUE)
# xtsplot(rs$ec, bench = "ecavg", main = "all equity curves")
#
#
#
# ##########################################################################\
#

stop("DEVEL.R End")

