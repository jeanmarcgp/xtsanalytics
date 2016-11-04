#
# test-summaries.R
# ----------------
#
# Test suite for testing functions in summaries.R
#
#
context("Testing function: perfstats()")

test_that("Testing basic functionality", {
  x    <- xts_data
  y    <- perfstats(x)

  expect_equal(y[1, 'SPY'], 7.00)
  expect_equal(y[1, 'BND'], 4.80)
  expect_equal(y[2, 'QQQ'], -53.39)
  expect_equal(y[4, 'EWN'], 0.05)
  expect_equal(y[3, 'VNQ'], 39.20)
  expect_equal(y[2, 'BND'], -9.31)

})


