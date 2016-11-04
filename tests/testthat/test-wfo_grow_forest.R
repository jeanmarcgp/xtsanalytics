#
# test-wfo_grow_forest.R
# ----------------------
#
# Test suite for testing function wfo_grow_forest
#
#
context("Testing function: wfo_grow_forest()")
library(testthat)

test_that("Testing basic functionality", {

  #----------------------------------------
  # First build a feature matrix
  #----------------------------------------
  prices     <- xts_data[, 1:2]
  featuresub <- c("y", "mom252", "sd21")
  features   <- c("mom252", "sd21")
  target     <- "mom63"    # 3 months

  smooth   <- list(mom63 = "ema5", sd21 = "sma5", mom252 = "sma10")



  #---------------------------------------------------------------------
  # Test for lookahead bias using weekly models.  Use two steps:
  #  1) test using data ending at 12/26
  #  2) test the same using data ending at 12/31
  #---------------------------------------------------------------------
  Nlags    <- -3
  prices2  <- prices["2013/2014-12-26", ]
  mlfeat   <- make_features(prices2, features, smooth = smooth, target = target)
  mlmat    <- make_featuremat(mlfeat, symbol = "SPY", featuresub = featuresub,
                              Nlags = Nlags, verbose = FALSE)
  set.seed(1)
  mlmat_sub <- mlmat["2014-12",]
  span      <- index(mlmat_sub[endpoints(mlmat_sub, on = "weeks")])
  x         <- wfo_grow_forest(featuremat = mlmat, modelwindow = 100, ylag = -Nlags[1],
                               wfo_span = span, ntree = 100, verbose = FALSE)


  #--------------------------------------------------------------------------------
  # STEP 1: model trained on Friday 12/19 at close, using most recent y data
  # shifted to 3 days earlier (vector on 12/16).  This allows to predict tomorrow
  # and beyond for the next week (12/22 to 12/26).
  #--------------------------------------------------------------------------------
  expect_equal(round(as.numeric(x$pred$yhat["2014-12-22"]), 4), 0.0052)
  expect_equal(round(as.numeric(x$pred$yhat["2014-12-26"]), 4), 0.0177)


  #--------------------------------------------------------------------------------
  # STEP 2: Same times as step 1, but use price subset extending to
  # 12/31 to see if we get the same thing.
  #--------------------------------------------------------------------------------
  prices2  <- prices["2013/2014-12-31", ]
  mlfeat   <- make_features(prices2, features, smooth = smooth, target = target)
  mlmat    <- make_featuremat(mlfeat, symbol = "SPY", featuresub = featuresub,
                              Nlags = Nlags, verbose = FALSE)
  set.seed(1)
  mlmat_sub <- mlmat["2014-12",]
  span      <- index(mlmat_sub[endpoints(mlmat_sub, on = "weeks")])
  x         <- wfo_grow_forest(featuremat = mlmat, modelwindow = 100, ylag = -Nlags[1],
                               wfo_span = span, ntree = 100, verbose = FALSE)

  expect_equal(round(as.numeric(x$pred$yhat["2014-12-22"]), 4), 0.0052)
  expect_equal(round(as.numeric(x$pred$yhat["2014-12-26"]), 4), 0.0177)

  sprint("wfo_grow_forest:  WRITE Monthly tests...")

  # jungle tests
  # modelwindow = 252
  # Nblocks = 7
  # ntree = 500
  # SPwindow = 63
  # featuremat <- mlmat



})


