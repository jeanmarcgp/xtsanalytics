#
# test-load_parameters.R
# ----------------------
#
# Test suite for testing function load_parameters
#
#
context("Testing function: load_parameters()")
library(testthat)

test_that("Testing basic functionality", {

  #print(getwd())
  fname <- "../../data/load_parameters test data file.xlsx"  # when run as tests
  #fname <- "./data/load_parameters test data file.xlsx"  # if run manually

  df <- load_parameters(fname)

  #print(df[1, ])
})

