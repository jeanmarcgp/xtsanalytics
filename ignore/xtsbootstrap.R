#
#  xtsbootstrap.R
#
#
#'
#'
#' Random size block bootstrapping for xts strategies (EMPTY)
#'
xtsboot <- function(data, FUN = NULL) {


}

#
#  DEMA 50 function code
DMAab = function(R, a) {
  alpha = 1/a
  result = emaTA(coredata(R), alpha, 0)
  result = emaTA(result, alpha, 0)
  result = result * 2200
  result = ifelse(result > -0.6, 1, 0)
  result = lag(result,-1, na.pad = T)
  result[1] = 1
  result
}



testfn <- function(data, arg) {
  rets <- ROC(data[, 1], type="discrete")
  rets$arg <- rets[, 1] * arg

  val <- cumprod_na(1 + rets$arg)
  return(val)
}
#
# R <- 1000
# #x <- xts_data[, 1]
# y <- ROC(xts_data[, 1])
# x <- cumprod_na(1 + y)
#
# x$v1  <- testfn(x, 0.3)
# x$v2  <- testfn(x, 0.5)
# xtsplot(x)
#
# z <- table.AnnualizedReturns(x, scale=252)
# b <- tsboot(x[, 1], statistic = mean, R = R, sim="fixed", l=10, n.sim=nrow(x))


