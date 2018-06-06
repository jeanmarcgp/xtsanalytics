#
# test-wfo_timeframe.R
# --------------------
#
# Test suite for testing function wfo_timeframe
#
#
context("Testing function: wfo_timeframe()")

test_that("Testing basic functionality", {

  gspc      <- xts_gspc["2000/2014", ]
  features  <- c("mom189", "mom126", "sdnaup126", "sdnadn126")
  target    <- "mom189"

  mlfeat      <- make_features(gspc, features, target = target)
  featuremat  <- make_featuremat(mlfeat, symbol = "GSPC", Nlags = 63)
  featuremat  <- na.trim(featuremat, sides = "left", is.na = "any")
  modelwindow <- 2009
  wfo_span    <- "months"
  ylag        <- 63
  earliest    <- NA
  verbose     <- FALSE

  ######  Basic test with earliest = NA
  x <- wfo_timeframe(featuremat = featuremat, modelwindow = modelwindow,
                     wfo_span = wfo_span, ylag = ylag, earliest = earliest,
                     verbose = verbose)

  expect_equal(x$wfo_start_date,   as.Date("2009-03-31"))
  expect_equal(x$Nlast,            3521)
  expect_equal(x$subset_timeframe, "2001-01-02/2014-12-31")
  expect_equal(x$wfo_timeframe,    "2009-03-31/2014-12-31")

  #######  WFO moves up one month due to lack of data
  modelwindow <- 2010
  x <- wfo_timeframe(featuremat = featuremat, modelwindow = modelwindow,
                     wfo_span = wfo_span, ylag = ylag, earliest = earliest,
                     verbose = verbose)

  expect_equal(x$wfo_start_date,   as.Date("2009-04-30"))
  expect_equal(x$Nlast,            3521)
  expect_equal(x$subset_timeframe, "2001-01-31/2014-12-31")
  expect_equal(x$wfo_timeframe,    "2009-04-30/2014-12-31")


  #######  date format test 1
  modelwindow <- 2009
  x <- wfo_timeframe(featuremat = featuremat, modelwindow = modelwindow,
                     wfo_span = wfo_span, ylag = ylag, earliest = "2012",
                     verbose = verbose)
  expect_equal(x$wfo_start_date, as.Date("2011-12-30"))


  #######  date format test 2
  x <- wfo_timeframe(featuremat = featuremat, modelwindow = modelwindow,
                     wfo_span = wfo_span, ylag = ylag, earliest = "2012-01-01",
                     latest = "2012", verbose = verbose)
  expect_equal(x$wfo_start_date,   as.Date("2011-12-30"))
  expect_equal(x$wfo_last_date,    as.Date("2012-12-31"))
  expect_equal(x$subset_timeframe, "2003-10-10/2012-12-31")


  #######  WFO moves up one month due to lack of data
  modelwindow  <- 2010
  x <- wfo_timeframe(featuremat = featuremat, modelwindow = modelwindow,
                     wfo_span = wfo_span, ylag = ylag, earliest = "2012-01-01",
                     latest = "2012", verbose = verbose)
  expect_equal(x$wfo_start_date,   as.Date("2011-12-30"))
  expect_equal(x$wfo_last_date,    as.Date("2012-12-31"))
  expect_equal(x$subset_timeframe, "2003-10-09/2012-12-31")

  #######  Test the warning message. First check for NA
  modelwindow  <- 2009
  x <- wfo_timeframe(featuremat = featuremat, modelwindow = modelwindow,
                     wfo_span = wfo_span, ylag = ylag, earliest = "2009-03-15",
                     latest = "2012", verbose = verbose)
  expect_true(is.na(x$warn_msg))
  expect_equal(x$wfo_last_date,    as.Date("2012-12-31"))
  expect_equal(x$subset_timeframe, "2001-01-02/2012-12-31")

  #######  Test the warning message. Now get a message
  modelwindow  <- 2009
  x <- wfo_timeframe(featuremat = featuremat, modelwindow = modelwindow,
                     wfo_span = wfo_span, ylag = ylag, earliest = "2009-02-28",
                     latest = "2012", verbose = verbose)
  expect_false(is.na(x$warn_msg))
  expect_equal(x$wfo_last_date,    as.Date("2012-12-31"))
  expect_equal(x$subset_timeframe, "2001-01-02/2012-12-31")


})

