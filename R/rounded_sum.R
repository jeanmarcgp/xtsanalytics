#====================================================================================
#
#  FUNCTION rounded_sum
#
#====================================================================================
#
#' Round a vector of numbers such that their sum stays.
#'
#' Applying the rounding function independently to a vector of numbers does
#' not, in general, preserve their sum.  This function ensures that the sum
#' is preserved after rounding.
#'
#' It achieves this using the following algorithm:
#' \enumerate{
#'    \item{Round down to the specified number of decimal places. }
#'    \item{Order numbers by their remainder values.}
#'    \item{Increment the specified decimal place of values with k largest
#'          remainders, where k is the number of values that must be
#'          incremented to preserve their rounded sum.
#'    }
#' }
#'
#'
#' @param  x       A numeric vector where it is desired that each number be
#'                 rounded to 'digits' digits.
#'
#' @param  digits  The number of digits where rounding is applied.
#'
#' @return Returns a vector similar to x, except that the numbers have been
#'         rounded and their sum is preserved.
#'
#' @export
#------------------------------------------------------------------------------------
rounded_sum <- function(x, digits) {

  #x = c(0.333, 0.333, 0.334)
  #round(x, 2)
  #digits = 2

  up      <- 10 ^ digits
  x       <- x * up
  y       <- floor(x)
  indices <- tail(order(x-y), round(sum(x)) - sum(y))
  y[indices] <- y[indices] + 1
  y / up

} #########  END rounded_sum  #########


