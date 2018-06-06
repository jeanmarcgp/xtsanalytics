#
# test-xtsoverlay.R
# -----------------------
#
# Test suite for testing function xtsoverlay
#
#
context("Testing function: xtsoverlay()")

test_that("Testing basic functionality", {

  library(xtsanalytics)

  spy <- xts_data["2007-01/2007-02", 1]

  dates_i     = endpoints(spy, on = "weeks")[-1]
  dates_dates = index(spy[dates_i, ])
  timezero    = dates_dates #[1]

  #--------------------------------------------------------
  # Test weekly offsets
  #--------------------------------------------------------
  ovlay <- xtsoverlay(spy, timezero = timezero, offsets = c(0, 10), norm = TRUE)

  ovlay <- xtsoverlay(spy, timezero = timezero, offsets = c(-2, 15), norm = TRUE)
  ovlay <- xtsoverlay(spy, timezero = timezero, offsets = c(0, 10), norm = FALSE)
  ovlay <- xtsoverlay(spy, timezero = timezero, offsets = c(-5, 12), norm = FALSE)
  #expect_equal(as.numeric(data["2007-12-21", 2]), 1)


})


