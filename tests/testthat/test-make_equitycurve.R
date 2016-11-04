#
# test-make_equitycurve.R
# -----------------------
#
# Test suite for testing function make_equitycurve
#
#
context("Testing function: make_equitycurve()")

test_that("Testing basic functionality", {

  data               <- xts_gspc["1998/2010", ]
  data$rets          <- TTR::ROC(data[, 1], type = "discrete")
  data$sma50sma200   <- TTR::SMA(data$GSPC, 50) / TTR::SMA(data$GSPC, 200) - 1
  data$GCtimer       <- ifelse(data$sma50sma200 > 0, 1, 0)
  data$GCtimerlag    <- lag(data$GCtimer)
  data$GCrets        <- data$GCtimerlag * data$rets
  data$GoldenCross   <- cumprod_na(1 + data$GCrets)

  datasub            <- c("GSPC", "GCtimerlag")
  data               <- data[, datasub]

  Nlag               <- 1
  rules              <- list(weeks  = list(type = "endpoints", on = "weeks", k = 1, offset = 0),
                             days   = list(type = "endpoints", on = "days"),
                             months = list(type = "endpoints", on = "months"),
                             weeks1 = list(type = "endpoints", on = "weeks", offset = -1),
                             weeks2 = list(type = "endpoints", on = "weeks", offset = -2)
                             )

  x <- make_equitycurve(data, rules = rules, Nlag = Nlag)

  #--------------------------------------------------------
  # Test daily, weekly and monthly ENDPOINTS alignment
  #--------------------------------------------------------
  expect_equal(as.numeric(data["2007-12-21", 2]), 1)
  expect_equal(as.numeric(data["2007-12-24", 2]), 0)

  expect_equal(as.numeric(x$timers["2007-12-24", "timer_days"]), 1) # Monday
  expect_equal(as.numeric(x$timers["2007-12-26", "timer_days"]), 0) # Wednesday

  expect_equal(as.numeric(x$timers["2007-12-28", "timer_weeks"]), 1) # Friday
  expect_equal(as.numeric(x$timers["2007-12-31", "timer_weeks"]), 0) # Monday

  expect_equal(as.numeric(x$timers["2007-12-27", "timer_weeks1"]), 1) # Thursday
  expect_equal(as.numeric(x$timers["2007-12-28", "timer_weeks1"]), 0) # Friday

  expect_equal(as.numeric(x$timers["2007-12-26", "timer_weeks2"]), 1) # Wed
  expect_equal(as.numeric(x$timers["2007-12-27", "timer_weeks2"]), 0) # Thurs

  expect_equal(as.numeric(x$timers["2007-12-31", "timer_months"]), 1) # Monday
  expect_equal(as.numeric(x$timers["2008-01-02", "timer_months"]), 0) # Wed




  # test that it aligns on mondays.  Also, add the adjust argument to
  # align on Fridays, thursdays etc. and test for the same
  # Also compare the transition from NA on the base timer and aligned on
  # using days and weeks with adjust.

})


