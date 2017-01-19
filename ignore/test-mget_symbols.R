#------------------------------------------------------------------
#
#  test-mget_symbols.R: Test suite for testing mget_symbols
#
#------------------------------------------------------------------
#
#
context("Testing function: mget_symbols")

test_that("Testing basic functionality of mget_symbols", {

  startdate  <- "2010-01-01"
  sym        <- c('SPY', 'QQQ', 'TLT', 'IEV')
  data       <- mget_symbols(sym, from = startdate)

  #expect_equal(as.numeric(round(data["2010-01-04", 'QQQ'], 2)), 43.38)  # bad data
  expect_equal(colnames(data), c('SPY', 'QQQ', 'TLT', 'IEV'))

  data       <- mget_symbols(sym, from=startdate, OHLC = "Cl")
  expect_equal(as.numeric(round(data["2010-01-04", 'QQQ'], 2)), 46.42)

  data       <- mget_symbols(sym, from=startdate, OHLC = "Op")
  expect_equal(as.numeric(round(data["2010-01-04", 'QQQ'], 2)), 46.33)

  data       <- mget_symbols(sym, from=startdate, OHLC = "Hi")
  expect_equal(as.numeric(round(data["2010-01-04", 'QQQ'], 2)), 46.49)

  data       <- mget_symbols(sym, from=startdate, OHLC = "Lo")
  expect_equal(as.numeric(round(data["2010-01-04", 'QQQ'], 2)), 46.27)

  startdate  <- "1990-01-01"
  sym        <- c('SPY', 'QQQ', 'TLT', 'IEV')
  data       <- mget_symbols(sym, from = startdate, OHLC = "Cl")

  expect_equal(as.numeric(round(data[1, 'SPY'], 2)), 43.94)  # 1993-01-29
  expect_equal(as.logical(data[1, 'QQQ']), NA)               # logical NA
  expect_equal(as.logical(data["1999-03-09", 'QQQ']), NA)    # logical NA
  expect_equal(as.numeric(round(data["1999-03-10", 'QQQ'], 2)), 102.12)

  #------------------------------------------------------------------
  # Test multiple OHLC functions
  #------------------------------------------------------------------
  ohlc       <- c("Cl", "Vo")
  data       <- mget_symbols(sym, from = startdate, OHLC = ohlc)
  expect_equal(as.numeric(first(data$Vo)$SPY), 1003200)




})


