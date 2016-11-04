#
# test-genpurpose.R
# -----------------
#
# Test suite for testing functions ins genpurpose.R
#
#
context("Testing function: split_vector()")

test_that("Testing basic functionality", {
  svec <- split_vector(c(1,2,3,4,5), 3)
  expect_equal(svec[[1]], c(1,2,3))
  expect_equal(svec[[2]], c(4,5))
  expect_equal(class(svec), "list")
  expect_equal(length(svec), 2)
})

context("Testing functions: cumsum_na() and cumprod_na()")

test_that("Testing basic functionality", {
  tvec1 <- c(NA, 1, 3, 2, NA, 0, 3)
  tvec2 <- c(NA, 1, 2, 1.2, 1, 5, NA, 1, 2)
  expect_equal(cumprod_na(tvec2), c(1, 1, 2, 2.4, 2.4, 12, 12, 12, 24))
  expect_equal(cumsum_na(tvec1), c(0, 1, 4, 6, 6, 6, 9))

})
