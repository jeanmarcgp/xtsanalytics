#
# test-make_featurecurve.R
# ------------------------
#
# Test suite for testing function make_featurecurve
#
#
context("Testing function: make_featurecurve()")

test_that("Testing basic functionality", {

  #### Test make_featurecurve
  timeframe  <- "1999/2010"
  features   <- c("mom252", "mom189", "mom126", "mom105",  "mom84", "mom63")
  Nlags      <- rep(-21, length(features))

  prices     <- xts_gspc["1997/2010", 1]

  mlfeat  <- make_features(prices, features)
  mlmat   <- make_featuremat(mlfeat, symbol = "GSPC", Nlags = Nlags)
  mlmat   <- mlmat[timeframe, ]

  prices  <- prices[timeframe, ]
  offsets <- list(sma50_sma200 = -1, `sd5(ema5)_sd5(sma50)` = -1)
  # x       <- make_featurecurve(prices, mlmat) #, offsets = offsets)
  #
  # expect_equal(round(as.numeric(x$long["2007-10-04", "mom252"]), 3), 1.813)
  #
  # Nlags   <- 1
  # x1      <- make_featurecurve(prices, mlmat, Nlags = Nlags)
  #
  # #  First transition on mom63 timers, no lag (x$timers)
  # expect_equal(as.numeric(x$timers["1999-05-11", "mom63"]), 1)
  # expect_equal(as.numeric(x$timers["1999-05-12", "mom63"]), 0)
  # expect_equal(as.numeric(x$timers["1999-05-13", "mom63"]), 0)
  #
  # #  First transition on mom63 timers, 1 day lag (x1$timers)
  # expect_equal(as.numeric(x1$timers["1999-05-11", "mom63"]), 1)
  # expect_equal(as.numeric(x1$timers["1999-05-12", "mom63"]), 1)
  # expect_equal(as.numeric(x1$timers["1999-05-13", "mom63"]), 0)


  #xtsplot(x$long, log = "y")


})


