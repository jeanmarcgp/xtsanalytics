#
# test-classification stats.R
# ----------------------------
#
# Test suite for testing functions in summaries.R
#
#
context("Testing function: periods_in_class()")

test_that("Testing basic functionality", {
  cat('testing periods_in_class function.\n')
  x    <- xts_data
  gc   <- make_bench(x[, "SPY"], type="Goldencross", retval=c("ec", "timer"))
  colnames(gc) <- c('ec', 'GC')
  gc <- na.locf(gc, na.rm=TRUE)

  gc2 <- periods_in_class(gc, col = "GC")

  expect_equal(as.numeric(gc2["2007-12-27", "cum_periods"]), 48.0)
  expect_equal(as.numeric(gc2["2007-12-28", "cum_periods"]), 1.0)

  gc3 <- periods_in_class(gc, col = 2, retcol="some_name")

  expect_equal(as.numeric(gc3["2007-12-27", "some_name"]), 48.0)
  expect_equal(as.numeric(gc3["2012-01-20", "some_name"]), 108.0)
  expect_equal(as.numeric(gc3["2012-01-23", "some_name"]), 1.0)


})

context("Testing function: predictor_stats()")

test_that("Testing basic functionality", {
  cat('testing predictor_stats function.\n')
  x    <- xts_gspc
  gc   <- make_bench(x[, "GSPC"], type="Goldencross", retval=c("ec", "timer"))
  colnames(gc) <- c('ec', 'GC')
  gc <- na.locf(gc, na.rm=TRUE)

  data <- predictor_stats(gc)
  #print(data)

  expect_equal(data$N_predictions, 64.0)
  expect_equal(as.numeric(data$predictions["1953-05-11", "TN"]), 0)
  expect_equal(as.numeric(data$predictions["1953-05-11", "FN"]), 1)
  expect_equal(as.numeric(data$predictions["1986-11-25", "actual"]), 1)
  expect_equal(as.numeric(data$predictions["2010-07-02", "ec"]), 60.68724)
  expect_equal(as.numeric(data$predictions["2011-08-12", "GC"]), 0)
  expect_equal(as.numeric(data$predictions["2011-08-12", "actual"]), 1)
  expect_equal(as.numeric(data$predictions["2011-08-12", "FN"]), 1)
  expect_equal(as.numeric(data$predictions["2009-06-23", "TP"]), 1)
  expect_equal(as.numeric(data$predictions["2010-10-22", "FP"]), 1)
  expect_equal(as.numeric(data$predictions["2007-12-21", "TN"]), 1)


})


