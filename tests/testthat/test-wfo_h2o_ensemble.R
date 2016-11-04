#
# test-wfo_h2o_ensemble.R
# -----------------------
#
# Test script to test the wfo_h2o_ensemble function
#
context("Testing function: wfo_h2o_ensemble()")

test_that("Testing basic functionality of wfo_h2o_ensemble()", {

  library(xtsanalytics)
  #----------------------------------------
  # First build a feature matrix
  #----------------------------------------
  prices     <- xts_data[, 1:2]
  featuresub <- c("y", "mom252", "sd21")
  features   <- c("mom252", "sd21")
  target     <- "mom63"    # 3 months

  smooth   <- NA



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

 #-----

  featuremat   = mlmat
  modelwindow  = 100
  wfo_span     = span
  ylag         = -Nlags[1]
  ntree        = 100
  verbose      = TRUE
  SPwindow     = 63
  jobname      = NA
  mtry         = 1
  importance   = TRUE
  earliest     = NA


  x         <- wfo_h2o_ensemble(featuremat = mlmat, modelwindow = 100,  ylag = -Nlags[1],
                                wfo_span = span, ntree = 100, verbose = TRUE)


  xrf       <- wfo_grow_forest(featuremat = mlmat, modelwindow = 100,  ylag = -Nlags[1],
                                wfo_span = span, ntree = 100, verbose = TRUE)

})
