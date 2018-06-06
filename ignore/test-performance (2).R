#
# test-performance.R
# ----------------------
#
# Test suite for testing function performance.R
#
#
context("Testing function: perfstats()")
#library(testthat)

test_that("Testing perfstats()", {

  # test single column
  ec <- xts_data[, 1]
  y  <- perfstats(ec, plotout = FALSE)
  expect_equal(round(y["Annualized Return (%)", "SPY"], 2),  7.00)
  expect_equal(round(y["Drawdown 3 (%)",        "SPY"], 2), -7.35)

  y  <- perfstats(ec, plotout = FALSE, top = 1)
  expect_equal(round(y["Annualized Return (%)", "SPY"], 2),  7.00)
  expect_equal(round(y["Max. Drawdown (%)",     "SPY"], 2), -55.19)

  # test with multiple columns / equity curves
  ec <- xts_data[, 1:2]
  y  <- perfstats(ec, plotout = FALSE)
  expect_equal(round(y["Annualized Return (%)", "VTI"], 2),  7.47)
  expect_equal(round(y["Drawdown 3 (%)",        "VTI"], 2), -9.47)

  y  <- perfstats(ec, plotout = FALSE, top = 1)
  expect_equal(round(y["Annualized Return (%)", "VTI"], 2),  7.47)
  expect_equal(round(y["Max. Drawdown (%)",     "VTI"], 2), -55.45)


})
