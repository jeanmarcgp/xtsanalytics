#
# test-optimize_statfolio.R
# -------------------------
#
#
library(xtsanalytics)
library(testthat)

context("Testing optimize_statfolio")

test_that("Testing optimize_statfolio function", {

  library(xtsanalytics)
  symbols     <- c("SPY", "BND", "VNQ", "IEV")
  maxbox      <- 0.5
  normfactor  <- 2.0        # daily momentum vs. daily SD
  feature     <- "mom189"   # Must specify daily momentum ALWAYS
  timeframe   <- "2011-07/2012"

  prices   <- xts_data[, symbols]
  returns  <- TTR::ROC(prices, type = "discrete")
  returns  <- returns[complete.cases(returns), ]
  funds    <- colnames(returns)


  #=================================================
  # Define custom objective function (momentum)
  #=================================================
  Momentum <- function(R, weights, featmat, ...) {
    subindex <- index(R)
    x        <- featmat[subindex, ]
    y        <- last(x)
    wgtmean  <- sum(as.numeric(y) * as.numeric(weights))
    return(wgtmean)

  }  ##### END Momentum #####


  #----------------------------------------------------------
  # Compute the momentum matrix, normalized to daily values
  # and scale by normfactor
  #----------------------------------------------------------
  featmat2 <- make_features(prices, features = feature)[[1]]
  featmat  <- (1 + featmat2)^(252 / 189) - 1
  featmat  <- featmat / sqrt(252) * normfactor

  # Subset all matrices by timeframe
  featmat  <- featmat[timeframe, ]
  prices   <- prices[timeframe, ]
  returns  <- returns[timeframe, ]

  #----------------------------------------------------------
  # Define baseline portfolio constraints
  #----------------------------------------------------------
  init.portfolio <- portfolio.spec(assets = funds)
  init.portfolio <- add.constraint(portfolio = init.portfolio, type = "leverage",
                                   min_sum = 0.99, max_sum = 1.01)
  init.portfolio <- add.constraint(portfolio = init.portfolio, type = "box",
                                   min = 0.0, max = maxbox)

  #----------------------------------------------------------
  # Define objective function
  #  - risk is the riskfcn (to minimize)
  #  - return is the objfcn (to maximize)
  #----------------------------------------------------------
  optport <- add.objective(portfolio = init.portfolio,
                           type = "risk", name = "StdDev")
  optport <- add.objective(portfolio = optport, type = "return",
                           name = "Momentum", arguments = list(featmat = featmat))


  #----------------------------------------------------------
  # Now call optimize_statfolio
  #----------------------------------------------------------
  res <- optimize_statfolio(returns, optport, train_window = 63, N = 1, traceDE = 3)

  print(res$allweights)
  print(res$SD_weights)

  ###### Need to figure out how to override the x and y labels  #######
  plot(res, risk.col = "StdDev", return.col = "Momentum", neighbors = 50)

  ####  Run again with seeds and expect_equal values  #####



  ###########################################
  #  Test wfo_statfolio
  ###########################################

  wfodata <- wfo_statfolio(returns, optport, N = 3, traceDE=5)
  chart.Weights(wfodata)

  save(list = c("wfodata"), file = "./ignore-results/wfodata.Rdata")


  #######################
  ####  Devel code  #####
  #######################
  library(xtsanalytics)
  load("./ignore-results/wfodata.Rdata")


  plot_statfolio((wfodata))


  })  ############## END testthat  #################


