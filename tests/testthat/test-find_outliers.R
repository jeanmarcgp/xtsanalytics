#
# test-find_outliers.R
# --------------------
#
# Test suite for testing function find_outliers
#
#
context("Testing function: find_outliers()")
library(testthat)

test_that("Testing basic functionality", {

  data <- xts_gspc[, 1]
  rets <- ROC(data, type = "discrete")

  find_outliers(as.data.frame(rets), GSPC)


})

