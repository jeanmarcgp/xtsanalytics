#
#
# test-random_strategy.R
# -----------------------
#
# Test suite for testing function random_strategy
#
#
context("Testing function: random_strategy()")

test_that("Testing basic functionality of random_strategy()", {

  library(xtsanalytics)
  library(microbenchmark)

  prices   <- xts_data[complete.cases(xts_data), ]
  prices   <- to.weekly(prices)
  data     <- TTR::ROC(prices, type = "discrete")
  N        <- 50
  K        <- 2
  weights  <- "uniform"

  rs     <- random_strategy(data, N = N, K = K, weights = weights, return_ec = TRUE)
  xtsplot(rs$ec, bench = "ecavg", main = "all equity curves")

  #rs     <- random_strategy(data, N = N, K = K, weights = weights, return_ec = FALSE)
  #xtsplot(rs$ec, bench = "ecavg")



})

