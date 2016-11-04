#
# test-make_predmat.R
# --------------------
#
# Test suite for testing function make_predmat
#
#
context("Testing function: make_predmat()")

test_that("Testing basic functionality of make_predmat()", {


  #### NOT YET IMPLEMENTED  ###



  prices <- xts_data[endpoints(xts_data["2007/2010"], on = "months"), ]
  rets   <- TTR::ROC(prices, type = "discrete")
  mom10  <- TTR::ROC(prices, n = 10)
  mom4   <- TTR::ROC(prices, n = 4)
  mom3   <- TTR::ROC(prices, n = 3)
  mom2   <- TTR::ROC(prices, n = 2)
  sd10   <- zoo::rollapplyr(rets, width = 10, FUN = sd)
  target <- TTR::ROC(prices, n = 3)

  #  Assign the matrices to a test environment for testthat to work.
  testing_env <- new.env()
  assign("mom10", mom10, envir = testing_env)
  assign("mom4",  mom4,  envir = testing_env)
  assign("sd10",  sd10,  envir = testing_env)
  assign("target", target, envir = testing_env)

  pnames <- c("mom10", "mom4", "sd10")
  # x      <- make_predmat(target, prednames = pnames, symbol = "QQQ", envir = testing_env)
  # x2     <- make_predmat(target, prednames = pnames, symbol = "QQQ",
  #                        Nlag = c(0,1,1,1), envir = testing_env)
  #
  # expect_equal(round(as.numeric(x["2007-12-31", "y"]), 2), 0.05)
  # expect_equal(round(as.numeric(x2["2008-02-29", "y"]), 4), -0.1768)
  #
  # expect_equal(round(as.numeric(x["2010-07-30", "mom10"]), 3), 0.087)
  # expect_equal(round(as.numeric(x2["2010-07-30", "mom10"]), 3), 0.072)
  #
  # expect_equal(round(as.numeric(x2["2010-11-30", "mom4"]), 3), 0.203)
  # expect_equal(round(as.numeric(x2["2010-11-30", "sd10"]), 3), 0.073)

})


