#
# test-make_featuremat.R
# --------------------
#
# Test suite for testing function make_featuremat
#
#
context("Testing function: make_featuremat()")

test_that("Testing basic functionality", {

  # for simple testing of make_features and make_featuremat
  prices     <- xts_data["2012-01-01/2012-03-31", 1:4]
  featuresub <- c("y", "mom5", "sd2", "mom21.sd21")
  features   <- c("mom5", "sd2", "mom21.sd21", "mom4", "sd3", "sma22", "ema32.sd4")
  target     <- "mom10"

  smooth     <- list(mom21 = "ema5", sd21 = "sma5", mom10 = "sma10")

  mlfeat     <- make_features(prices, features, smooth = smooth, target = target)

  # Test that y in mlmat is mlfeat$y[, "SPY_sma10"] lagged by one day
  mlmat      <- make_featuremat(mlfeat, symbol = "SPY", featuresub = featuresub)
  expect_equal(as.numeric(mlfeat$y["2012-03-29", "SPY_sma10"]), as.numeric(mlmat$y["2012-03-30"]))

  # Test that y in mlmat is mlfeat$y[, "BND_sma10"] lagged by one day
  mlmat  <- make_featuremat(mlfeat, symbol = "BND")
  expect_equal(as.numeric(mlfeat$y["2012-03-29", "BND_sma10"]), as.numeric(mlmat$y["2012-03-30"]))

  # Test that other features are properly lined up
  testdate <- "2012-03-28"
  ticker   <- "BND"
  expect_equal(as.numeric(mlfeat$mom5[testdate, ticker]), as.numeric(mlmat$mom5[testdate]))
  expect_equal(as.numeric(mlfeat$sd2[testdate, ticker]), as.numeric(mlmat$sd2[testdate]))
  expect_equal(as.numeric(mlfeat$mom21.sd21[testdate, "BND_ema5"]), as.numeric(mlmat$mom21.sd21[testdate]))


  #--------------------------------------
  # Redo tests for 21 day lag of y
  #--------------------------------------
  mlmat <- make_featuremat(mlfeat, symbol = "VNQ", Nlags = c(21, 0, 0, 0), featuresub = featuresub)

  # Test that y in mlmat is mlfeat$y[, "VNQ_sma10"] lagged by 21 days
  expect_equal(as.numeric(mlfeat$y["2012-03-01", "VNQ_sma10"]), as.numeric(mlmat$y["2012-03-30"]))

  # Test that other features are properly lined up
  testdate <- "2012-03-28"
  ticker   <- "VNQ"
  expect_equal(as.numeric(mlfeat$mom5[testdate, ticker]), as.numeric(mlmat$mom5[testdate]))
  expect_equal(as.numeric(mlfeat$sd2[testdate, ticker]), as.numeric(mlmat$sd2[testdate]))
  expect_equal(as.numeric(mlfeat$mom21.sd21[testdate, "VNQ_ema5"]), as.numeric(mlmat$mom21.sd21[testdate]))


})


