#
# test-rebalance.R
# ----------------
#
# Test suite for testing teh rebalance function
#
#
library(xtsanalytics)

prices    <- xts_data["2007/2012", c("SPY", "BND")]
weights   <- list(SPY = 0.60, BND = 0.40)
ec        <- rebalance(prices, weights, on = "months")[, "ec"]
colnames(ec) <- "months"
ec$quarters  <- rebalance(prices, weights, on = "quarters")[, "ec"]
ec$years     <- rebalance(prices, weights, on = "years")[, "ec"]

allec <- xtsbind(ec, prices)
allec <- allec[complete.cases(allec), ]


xtsplot(allec)

# context("Testing function: perfstats()")
#
# test_that("Testing basic functionality", {
#   x    <- xts_data
#   y    <- perfstats(x)
#
#   expect_equal(y[1, 'SPY'], 7.00)
#   expect_equal(y[1, 'BND'], 4.80)
#   expect_equal(y[2, 'QQQ'], -53.39)
#   expect_equal(y[4, 'EWN'], 0.05)
#   expect_equal(y[3, 'VNQ'], 39.20)
#   expect_equal(y[2, 'BND'], -9.31)
#
# })


