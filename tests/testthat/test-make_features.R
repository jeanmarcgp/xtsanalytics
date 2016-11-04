#
# test-make_features.R
# --------------------
#
# Test suite for testing function make_predmat
#
#
context("Testing function: make_features()")

test_that("Testing basic functionality of make_features()", {

  features <- c("mom5", "sd5", "mom21", "mom7.sd7", "mom3.sd2(sma5)")
  target   <- "mom10"
  prices   <- xts_data["2012-01-01/2012-03-31", 1:4]
  rets     <- TTR::ROC(prices, type = "discrete")

  smooth   <- list(mom21 = "ema5", sd7 = "sma3")

  mlfeat <- make_features(prices, features, smooth = smooth, target = target)

  # Test that mom10 for y works fine
  expect_equal(round(as.numeric(mlfeat$y["2012-01-18", "SPY"]), 3), 0.026)
  expect_equal(round(as.numeric(mlfeat$y["2012-01-18", "BND"]), 3), 0.004)

  # Test that mom5 of prices works ok
  expect_equal(round(as.numeric(mlfeat$mom5["2012-01-10", "VTI"]), 3), 0.015)

  # Test that sd5 works as expected
  expect_equal(round(as.numeric(mlfeat$sd5["2012-01-10", "SPY"]), 3), 0.004)

  # Test that mom21 from ema5 of prices works ok
  emaprices   <- prices
  emaprices[] <- apply(prices, 2, TTR::EMA, n = 5)
  expect_equal(round(as.numeric(mlfeat$mom21["2012-03-30", "VNQ_ema5"]), 3), 0.039)


  # Test that mom7.sd7, where the prices used to compute sd7 are smoothed using sma3
  mom7val <- as.numeric(prices["2012-03-30", "SPY"]) / as.numeric(prices["2012-03-21", "SPY"]) - 1
  mom7    <- make_transform(prices, "mom7", addname = TRUE)
  expect_equal(round(mom7val, 4), round(as.numeric(mom7["2012-03-30", "SPY_mom7"]), 4))

  sma3    <- make_transform(prices, "sma3", addname = TRUE)
  sma3val <- mean(prices["2012-03-28/2012-03-30", "VTI"])
  expect_equal(round(sma3val, 2), round(as.numeric(sma3["2012-03-30", "VTI_sma3"]), 2))

  sma3rets <- TTR::ROC(sma3, type = "discrete")
  sd7      <- make_transform(sma3rets, "sd7", addname = TRUE)

  # manually calculate the mom7.sd7 value to compare
  result <- mom7val * as.numeric(sd7["2012-03-30", "SPY_sma3_sd7"])

  expect_equal(signif(result, 3), signif(as.numeric(mlfeat$mom7.sd7["2012-03-30", "SPY"]), 3))

  # Test mom3.sd2(sma5)
  mom3      <- make_transform(prices, "mom3", addname = TRUE)
  sd2       <- make_transform(rets, "sd2", addname = TRUE)
  sd2sma5   <- prices
  sd2sma5[] <- apply(sd2, 2, FUN = TTR::SMA, n=5)
  results <- mom3 * sd2sma5

  res1 <- as.numeric(results["2012-03-30", "VNQ_mom3"])
  res2 <- as.numeric(mlfeat$`mom3.sd2(sma5)`["2012-03-30", "VNQ"])
  expect_equal(signif(res1, 4), signif(res2, 4))

  # Test subsetting the features using on = "weeks"


  # Test the Lag via postprocessing (formerly just smoothing)
  # NOTE:  Lag overrides Nlags, so if Nlags = 1, this is the same
  # as <feature>(lag1)
  features <- c("mom10", "sd10", "mom10(lag1)")
  prices   <- xts_data["2012-01-01/2012-03-31", 1:4]
  rets     <- TTR::ROC(prices, type = "discrete")

  smooth   <- NA

  mlfeat   <- make_features(prices, features, smooth = smooth, target = NA)
  mlmat    <- make_featuremat(mlfeat, symbol = "SPY", Nlags = 0)

})

context("Testing function: make_transform()")
test_that("Base functionality", {

  prices   <- xts_data["2012-01-01/2012-03-31", 1:4]
  rets     <- TTR::ROC(prices, type = "discrete")

  x <- make_transform(rets, "sd2", addname = TRUE)

  expect_equal(colnames(x)[3], "BND_sd2")
  expect_equal(round(as.numeric(x["2012-01-05", "SPY_sd2"]), 5), 0.00076)

  x <- make_transform(prices, "sma5", addname = FALSE)
  expect_equal(round(as.numeric(x["2012-01-09", "VTI"]), 3), 61.618)

  x <- make_transform(prices, "ema3", addname = TRUE)
  expect_equal(round(as.numeric(x["2012-01-09", "BND_ema3"]), 3), 76.929)

  # Test with post result smoothing
  x2 <- make_transform(rets, "sd3", addname = TRUE)
  x <- make_transform(rets, "sd3(sma4)", addname = TRUE)
  expect_equal(round(as.numeric(x["2012-01-11", "VNQ_sd3(sma4)"]), 3),0.009)

})

#
# library(xtsanalytics)
#
# prices <- xts_data[, c(1,3)]
# feat   <- make_features(prices, "tailratio126")[[1]]
# both   <- xtsbind(prices, feat)
#
# xtsplot(both)
