#
#
# test-xtsplot.R
# --------------
#
# Test suite for testing the xtsplot function.
#
#
context("Testing function: xtsplot()")

test_that("Testing argument values", {
  expect_error(xtsplot(c(1,2)))
  expect_error(xtsplot(type="blabla"))
  expect_error(xtsplot(theme='nothing'))

})
