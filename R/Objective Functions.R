#
# Momentum.R
#
#
#  Rename this file to Objective Functions.R
#   put in the various objective functions needed...
#
#--------------------------------------------
# Custom objective function definition
# Assumes objfnmat is an xts of momentums
#--------------------------------------------
#'
#'
#'
#' function Momentum
#' @export
Momentum <- function(R, weights, objfnmat = NA, ...) {

    subindex <- last(index(R))
    y        <- objfnmat[subindex, ]
    wgtmean  <- sum(as.numeric(y) * as.numeric(weights))
    return(wgtmean)


}  ##### END Momentum FUNCTION #####
