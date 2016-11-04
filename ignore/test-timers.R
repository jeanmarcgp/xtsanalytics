#
# test-features.R
# ---------------
#
# Test suite for testing functions in features.R
#
#
context("Testing function: funapply()")

test_that("Testing basic functionality", {
  x <- funapply(xts_data[, 'VTI'], FUN=sd, windows=c(20,40,100))
  expect_error(funapply(c(1,2)))
  expect_equal(round(as.numeric(x["2014-12-23", "sd40"]),3), 1.525)

})

test_that("Testing additional arguments", {
  #  Test funapply with multiple arguments
  fn <- function(data, arg1) {
    retval <- mean(data)
    if(arg1) retval <- retval + 10
    return(retval)
  }

  vec <- c(1,2,3,4,5,6,7,8,9)
  data <- xts(vec, order.by=index(xts_data[1:length(vec)]))
  x <- funapply(data, FUN=fn, windows=4, arg1=FALSE)
  y <- funapply(data, FUN=fn, windows=4, arg1=TRUE)

  expect_equal(round(as.numeric(x["2007-01-08", 1]), 1), 2.5)
  expect_equal(round(as.numeric(y["2007-01-08", 1]), 1), 12.5)


})

#------------------------------------------------------------------------------------------
#  TESTS FOR:  make_timer()
#------------------------------------------------------------------------------------------
context("Testing function: make_timer()")

test_that("Testing basic functionality", {
  x <- xts_data[, 'VTI']
  x$rets <- ROC(x[,1], type="discrete")
  x$indicator <- funapply(x$rets, FUN=mean, windows=20)
  y <- make_timer(x, thresh = 0, retval=c('timedrets', 'timerlag', 'timer', 'timedec', 'ec', 'rets'))

  expect_equal(as.numeric(y["2014-12-24", 2]) == 1, TRUE)
  expect_equal(as.numeric(round(y["2014-12-24", 1], 4)) == 0.0003, TRUE)

  expect_error(z <- make_timer(x, retval = 'blah'))
  expect_error(z <- make_timer(x, retval = NULL))
  expect_error(z <- make_timer(x, multipliers = 'blah'))

  expect_error(z <- make_timer(x, multipliers = 1 ))
  expect_error(z <- make_timer(x, cnames = 'blah'))

  #  Tests to ensure it lags one period - Look at timer & timerlag change.
  alldata <- cbind(x, y)
  expect_equal(as.numeric(round(alldata["2014-12-10", 'rets'], 4)) == -0.0164, TRUE)
  expect_equal(as.numeric(round(alldata["2014-12-10", 'indicator_timedrets'], 4)) == -0.0164, TRUE)

  expect_equal(as.numeric(alldata["2014-12-09", 'indicator_timerlag']) == 1, TRUE)
  expect_equal(as.numeric(alldata["2014-12-09", 'indicator_timer']) == 1, TRUE)

  expect_equal(as.numeric(alldata["2014-12-10", 'indicator_timerlag']) == 1, TRUE)
  expect_equal(as.numeric(alldata["2014-12-10", 'indicator_timer']) == 0, TRUE)

  expect_equal(as.numeric(alldata["2014-12-11", 'indicator_timerlag']) == 0, TRUE)
  expect_equal(as.numeric(alldata["2014-12-11", 'indicator_timer']) == 0, TRUE)

  #  Ensure the equity curve is computed right.
  expect_equal(as.numeric(round(alldata["2014-12-10", 'indicator_timedec'], 4)) == 1.5601, TRUE)
  expect_equal(as.numeric(round(alldata["2014-12-10", 'ec'], 4)) == 1.7440, TRUE)

})

test_that("Testing Golden Cross on SPY", {
  x        <- xts_data[, 'SPY']
  x$rets   <- ROC(x[,1], type="discrete")
  x$sma50  <- funapply(x$SPY, FUN=mean, windows=50)
  x$sma200 <- funapply(x$SPY, FUN=mean, windows=200)
  x$GC     <- x$sma50 / x$sma200
  y <- make_timer(x, retval=c('rets', 'timedrets', 'ec', 'timedec' ), thresh = 1, cols = c('rets', 'GC'))

  x$sma50_2  <- TTR::SMA(x$SPY, 50)
  x$sma200_2 <- TTR::SMA(x$SPY, 200)
  x$GC2      <- x$sma50_2 / x$sma200_2
  y2 <- make_timer(x, retval=c('rets', 'timedrets', 'ec', 'timedec' ), thresh = 1,
                   cols = c('SPY', 'GC2'), seriestype = 'prices')

  #xtsplot(y2[, c('ec', 'GC2_timedec')])

  #  Ensure the equity curve is computed right.
  expect_equal(as.numeric(round(y["2014-12-31", 'GC_timedec'], 4)) == 1.9355, TRUE)
  expect_equal(as.numeric(round(y["2014-12-31", 'ec'], 3)) == 1.717, TRUE)

})

test_that("Testing rolling vote in timer", {

  x        <- xts_data[, 'SPY']
  x$rets   <- ROC(x[,1], type="discrete")
  x$sma50  <- funapply(x$SPY, FUN=mean, windows=50)
  x$sma200 <- funapply(x$SPY, FUN=mean, windows=200)
  x$GC     <- x$sma50 / x$sma200

  y        <- make_timer(x, retval=c('timer'), thresh = 1,  cols = c('rets', 'GC'))
  y$v5_5   <- make_timer(x, retval=c('timer'), thresh = 1,  cols = c('rets', 'GC'), vote = c(5,5))
  y$v15_5 <- make_timer(x, retval=c('timer'), thresh = 1,  cols = c('rets', 'GC'), vote = c(15,5))
  #xtsplot(y["2007/2009",])

  #print(y["2007-12/2008-01", ])
  #print(y["2009-06/2009-07", ])

  #  Golden Cross going from 1 -> 0
  expect_equal(as.numeric(y$GC_timer["2007-12-27"]) == 1, TRUE)
  expect_equal(as.numeric(y$GC_timer["2007-12-28"]) == 0, TRUE)
  expect_equal(as.numeric(y$v5_5["2008-01-04"]) == 1, TRUE)
  expect_equal(as.numeric(y$v5_5["2008-01-07"]) == 0, TRUE)
  expect_equal(as.numeric(y$v15_5["2008-01-04"]) == 1, TRUE)
  expect_equal(as.numeric(y$v15_5["2008-01-07"]) == 0, TRUE)


  #  Golden Cross going from 0 -> 1
  expect_equal(as.numeric(y$GC_timer["2009-06-17"]) == 0, TRUE)
  expect_equal(as.numeric(y$GC_timer["2009-06-18"]) == 1, TRUE)

  expect_equal(as.numeric(y$v5_5["2009-06-24"]) == 0, TRUE)
  expect_equal(as.numeric(y$v5_5["2009-06-25"]) == 1, TRUE)

  expect_equal(as.numeric(y$v15_5["2009-07-09"]) == 0, TRUE)
  expect_equal(as.numeric(y$v15_5["2009-07-10"]) == 1, TRUE)


})

test_that("Ensure column names are as expected", {
  x         <- xts_data[, 'SPY']
  x$returns <- ROC(x[,1], type="discrete")
  x$sma200  <- funapply(x$SPY, FUN=mean, windows=200)
  x$cross   <- x$SPY / x$sma200
  y1        <- make_timer(x, retval=c('ec', 'rets', 'timedec'), thresh = 1,
                          cols = c('returns', 'cross'),
                          seriestype = 'rets', vote = c(0,20))

  y2        <- make_timer(x, retval=c('ec', 'rets', 'timedec'), thresh = 1,
                          cols = c('SPY', 'cross'),
                          seriestype = 'prices', vote = c(0,25))

  cy1 <- colnames(y1)
  cy2 <- colnames(y2)

  expect_equal(cy1[1] == "ec",            TRUE)
  expect_equal(cy1[2] == "returns",       TRUE)
  expect_equal(cy1[3] == "cross_timedec", TRUE)

  expect_equal(cy2[1] == "ec",            TRUE)
  expect_equal(cy2[2] == "rets",          TRUE)
  expect_equal(cy2[3] == "cross_timedec", TRUE)

#
#   colnames(y1)
#   print(y1["2014-12",])
#   xtsplot(y1[, c('ec', 'cross_timedec')])
#
#   print(y2["2014-12",])
#   xtsplot(y2[, c('ec', 'cross_timedec')])

})

#------------------------------------------------------------------------------------------
#  TESTS FOR:  make_bench()
#------------------------------------------------------------------------------------------
context("Testing function: make_bench()")

test_that("Testing basic functionality", {
  x        <- xts_data[, 'SPY']
  x$GC     <- make_bench(xts_data[, 'SPY'], type = 'Goldencross')
  x$MD     <- make_bench(xts_data[, 'SPY'], type = 'Minidipper')
  x$F10    <- make_bench(xts_data[, 'SPY'], type = 'Faber10', on='months')
  x$dema50 <- make_bench(xts_data[, 'SPY'], type = 'Dema50')
  #xtsplot(x)

  expect_equal(as.numeric(round(x["2014-12-24", 'SPY'],    2)) == 207.77, TRUE)
  expect_equal(as.numeric(round(x["2014-12-24", 'GC'],     2)) == 1.96, TRUE)
  expect_equal(as.numeric(round(x["2014-12-24", 'F10'],    2)) == 2.28, TRUE)
  expect_equal(as.numeric(round(x["2014-12-24", 'dema50'], 2)) == 2.06, TRUE)

  #  Test offsets
  x    <- xts_data[, 'SPY']
  x$GC     <- make_bench(xts_data[, 'SPY'], type = 'Goldencross', offset =12)
  x$MD     <- make_bench(xts_data[, 'SPY'], type = 'Minidipper',  offset = 12)
  x$F10    <- make_bench(xts_data[, 'SPY'], type = 'Faber10', offset = 12, on='months')
  x$dema50 <- make_bench(xts_data[, 'SPY'], type = 'Dema50', offset = 12)
  #xtsplot(x)

  expect_equal(as.numeric(round(x["2014-12-24", 'SPY'],    2)) == 207.77, TRUE)
  expect_equal(as.numeric(round(x["2014-12-24", 'GC'],     2)) == 1.96, TRUE)
  expect_equal(as.numeric(round(x["2014-12-24", 'F10'],    2)) == 1.70, TRUE)
  expect_equal(as.numeric(round(x["2014-12-24", 'dema50'], 2)) == 2.05, TRUE)


  y        <- xts_gspc['1990/2014', ]
  y$GC     <- make_bench(y[, 'GSPC'], type = 'Goldencross')
  y$F10    <- make_bench(y[, 'GSPC'], type = 'Faber10', on='months')
  y$MD     <- make_bench(y[, 'GSPC'], type = 'Minidipper')

  #y$dema50 <- make_bench(y[, 'GSPC'], type = 'Dema50')
  #xtsplot(y)

  expect_equal(as.numeric(round(y["2014-12-31", 'GSPC'], 2)) == 2058.90, TRUE)
  expect_equal(as.numeric(round(y["2014-12-31", 'GC'],   4)) == 7.8675, TRUE)
  expect_equal(as.numeric(round(y["2014-12-31", 'F10'],  4)) == 8.9219, TRUE)
  #expect_equal(as.numeric(round(y["2014-12-31", 'MD'],   4)) == 7.1497, TRUE)

  #  Test the rolling vote scheme...
  z        <- xts_gspc['1990/2014', ]
  z$GC     <- make_bench(z[, 'GSPC'], type = 'Goldencross', retval = 'timer')
  z$GCv3_7 <- make_bench(z[, 'GSPC'], type = 'Goldencross', retval = 'timer', vote = c(3, 7))
  z$GCv0_0 <- make_bench(z[, 'GSPC'], type = 'Goldencross', retval = 'timer', vote = c(0, 0))

  #print(z["2007-12/2008-01",])
  #print(z["2009-06/2009-07",])

  #  Golden Cross going from 1 -> 0
  expect_equal(as.numeric(z$GC["2007-12-20"]) == 1, TRUE)
  expect_equal(as.numeric(z$GC["2007-12-21"]) == 0, TRUE)
  expect_equal(as.numeric(z$GCv0_0["2007-12-20"]) == 1, TRUE)
  expect_equal(as.numeric(z$GCv0_0["2007-12-21"]) == 0, TRUE)
  expect_equal(as.numeric(z$GCv3_7["2008-01-02"]) == 1, TRUE)
  expect_equal(as.numeric(z$GCv3_7["2008-01-03"]) == 0, TRUE)

  #  Golden Cross going from 0 -> 1
  expect_equal(as.numeric(z$GC["2009-06-22"]) == 0, TRUE)
  expect_equal(as.numeric(z$GC["2009-06-23"]) == 1, TRUE)
  expect_equal(as.numeric(z$GCv0_0["2009-06-22"]) == 0, TRUE)
  expect_equal(as.numeric(z$GCv0_0["2009-06-23"]) == 1, TRUE)
  expect_equal(as.numeric(z$GCv3_7["2009-06-25"]) == 0, TRUE)
  expect_equal(as.numeric(z$GCv3_7["2009-06-26"]) == 1, TRUE)

  #  Test the rolling vote scheme using Faber10...
  z2        <- xts_gspc['1990/2014', ]
  z2        <- xts_gspc[, ]

  #z2$f     <- make_bench(z2[, 'GSPC'], type = 'Faber10', retval = 'timer')
  z2$faber      <- make_bench(z2[, 'GSPC'], type = 'Faber10', retval = 'timedec', vote = c(0, 0))
  z2$faber0_20  <- make_bench(z2[, 'GSPC'], type = 'Faber10', retval = 'timedec', vote = c(0, 20))
  z2$faber0_21 <- make_bench(z2[, 'GSPC'], type = 'Faber10', retval = 'timedec', vote = c(0, 21))
  z2$faberv0_18 <- make_bench(z2[, 'GSPC'], type = 'Faber10', retval = 'timedec', vote = c(0, 18))
  z2$faberv0_23 <- make_bench(z2[, 'GSPC'], type = 'Faber10', retval = 'timedec', vote = c(0, 23))


  #cat("\n  >>> NEED to ADD FABER tests in make_bench().\n\n")



})

