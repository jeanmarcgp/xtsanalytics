#
#  FILE rand_port.R
#
###################################
#
#' Generates N random portfolios based on an asset universe
#'
#'
#' This function is slow for 1000 random portfolios.  May need to look at Rcpp
#' since bytecode compilation doesn't seem to help, and neither using lapply to
#' get rid of the for loop.
#'
#' @param data       An xts matrix of periodic returns (NOT prices).  These
#'                   may be daily, weekly, monthly, quarterly or yearly returns.
#'
#' @param N          The number of random portfolios to generate
#'
#'
#' @param Kassets    The number of asset returns randomly picked (without replacement)
#'                   at each time period to build each random strategy equity curve.
#'
#' @param weights    Specifies how the asset weights will be assigned in each random
#'                   portfolio. Set to "equal" to specify equal weighting.  Set to
#'                   "uniform" to specify a uniform random distribution for each asset.
#'
#' @param return_ec  Logical.  If set to TRUE, all randomly generated equity curves
#'                   are returned via an object $ec in the returned list. Otherwise,
#'                   only the average equity curve is returned in $ec, named 'ecavg'.
#'
#'
#' @return   Returns a list of statistics and matrices related to the random portfolios generated
#'           as follows:
#' \describe{
#'   \item{\preformatted{$cagr, $mdd, $mar:}}{
#'     Each of these is a numeric vector containing the following statistics for the performance metric:
#'     CAGR (the annualized return), the Maximum Drawdown (MDD) and the MAR ratio (CAGR / MDD).
#'     \itemize{
#'        \item
#'        \strong{mean     } The average (mean) CAGR over the distribution of portfolios.
#'        \item
#'        \strong{SD       } The standard deviation of CAGRs over the distribution of portfolios.
#'        \item
#'        \strong{skewness } The skewness of CAGRs over the distribution of portfolios.
#'        \item
#'        \strong{kurtosis } The kurtosis of CAGRs over the distribution of portfolios.
#'        \item
#'        \strong{quantiles} The next three items are, respectively:  First Quartile,
#'                           Median and Thrid Quartile.
#'     }
#'   }
#'
#'   \item{\preformatted{$ec }}{
#'     An xts matrix containing the average equity curve of all random portfolio, named ecavg, if
#'     return_ec = FALSE.  If return_ec = TRUE, then all random portfolio equity curves are included,
#'     in addition to ecavg.
#'     }
#'
#'   \item{\preformatted{$cagr_dist, $mdd_dist, $mar_dist: }}{
#'     Each of these is a named numeric vector containing the value of their statistic for each
#'     equity curve.  These are useful to visualize the distribution of the statistic.
#'     }
#'
#' }
#'
#' @seealso quantmod::endpoints()
#' @export
#--------------------------------------------------------------------------------
#################################################
# This version is, in fact NOT slower than
# the version with byte compiler. Needs Rcpp!
#################################################
rand_port <- function(data, N = 50, Kassets = 1, weights = "equal",
                      return_ec = FALSE) {

  cnames <- colnames(data)
  nc     <- ncol(data)

  ecnames <- paste0("ec", 1:N)
  ecrets  <- emptyxts(cnames = ecnames, order.by = index(data))

  cnames <- paste0("Asset_", 1:K)
  rmat   <- emptyxts(cnames = cnames, order.by = index(data))
  wmat   <- rmat

  # Set up equal weights in wmat holder
  ew     <- 1 / ncol(wmat)
  wmat[] <- rep(ew, length(wmat))

  #-------------------------------------------
  # MAIN LOOP for N portfolios
  #  Quite slow but need Rcpp to accelerate
  #  Byte code compile doesn't help!
  #-------------------------------------------
  for(i in 1:N) {
    #--------------------------------------------
    # Build random returns and weights matrices
    #--------------------------------------------
    rmat[] <- t(apply(data, 1, function(x) sample(x, size = K, replace = FALSE)))

    if(weights == "uniform") {
      # Compute uniform random weights, else just keep wmat as is
      wmat[] <- runif(length(wmat), min = 0, max = 1)
      wmat[] <- wmat / apply(wmat, 1, sum)
    }

    #--------------------------------------
    # Compute equity curve (as returns)
    #--------------------------------------
    eca          <- rmat * wmat
    ecrets[, i]  <- apply(eca, 1, sum)
  }

  #-------------------------------------------------
  # Calculate statistics to report
  #-------------------------------------------------
  # These functions are very fast!
  ecrets$ecavg <- as.numeric(apply(ecrets, 1, mean))
  ec           <- cumprod_na(1 + ecrets) # very fast!
  ecavg        <- ec[, "ecavg", drop = FALSE]

  # remove ecavg from the distributions to guarantee no biasing
  ec           <- ec[,     -ncol(ec)]
  ecrets       <- ecrets[, -ncol(ec)]

  cagr        <- as.numeric(xtscagr(ec))
  names(cagr) <- colnames(ec)

  allmdd        <- as.numeric(xtsmdd(ecrets))
  names(allmdd) <- colnames(ec)
  allmar <- cagr / -allmdd

  # Calculate quartiles to report
  cagr_quart  <- quantile(cagr, probs = c(0.25, 0.5, 0.75))
  mdd_quart  <- quantile(allmdd, probs = c(0.25, 0.5, 0.75))
  mar_quart  <- quantile(allmar, probs = c(0.25, 0.5, 0.75))

  cagr_vec <- c(mean(cagr),   sd(cagr),   skewness(cagr),   kurtosis(cagr),   cagr_quart)
  mdd_vec  <- c(mean(allmdd), sd(allmdd), skewness(allmdd), kurtosis(allmdd), mdd_quart)
  mar_vec  <- c(mean(allmar), sd(allmar), skewness(allmar), kurtosis(allmar), mar_quart)

  vecnames <- c("Mean", "SDev", "Skewness", "Kurtosis",
                "First Quart.", "Median", "Third Quart.")
  names(cagr_vec) <- vecnames
  names(mdd_vec)  <- vecnames
  names(mar_vec)  <- vecnames

  if(return_ec) retec <- xtsbind(ecavg, ec) else
    retec <- ecavg

  retlist <- list(ec        = retec,
                  cagr      = cagr_vec,
                  mdd       = mdd_vec,
                  mar       = mar_vec,
                  cagr_dist = cagr,
                  mdd_dist  = allmdd,
                  mar_dist  = allmar)

  return(retlist)

}




#-------------------------------------------------------------------------------------
#  ATTEMPT TO ACCELERATE rand_port below.
#
#  This version does not, in fact, perform any faster, but is more complicated.
#  I tried to preallocate the data in lists but it didn't gain any speed.
#  using the byte compiler for some of these efforts didn't accelerate either.
#  Going back to try to optimize the original code instead.
#
#  I may need to resort to Rcpp if all else fails.
#-------------------------------------------------------------------------------------
#' @export
rand_port2 <- function(data, N = 50, Kassets = 1,
                       weights = "equal") {


  cnames <- colnames(data)
  nc     <- ncol(data)

  #-------------------------------------------------------------------
  #  Create data structure holders:
  #  . A list of N portfolio structures
  #  . Each portfolio structure contains three K wide xts:
  #     . A returns xts for K sampled assets (arets)
  #     . A weights xts for K samples assets (aweights)
  #     . An xts multiplying rets * weights pairwise (aec)
  #  . A separate N wide xts (pecr) to store the sum of each aec
  #    row-wise i.e. Each portfolio's final return
  #-------------------------------------------------------------------
  # Create N wide xts pec to store each equity curve
  ecnames   <- paste0("ec", 1:N)
  pecr      <- emptyxts(cnames = ecnames, order.by = index(data))

  # Create prototype xts to construct single random portfolio
  anames    <- paste0("Asset_", 1:K)
  arets     <- emptyxts(cnames = anames, order.by = index(data))
  aec       <- arets    # same structure as arets

  # Put equal weights in prototype holder xts aweights
  aweights   <- arets    # same structure as arets
  ew         <- 1 / ncol(aweights)
  aweights[] <- rep(ew, length(aweights))



  # Create a single list containing all three xts above
  single_list <- list(ec = list(arets = arets, aweights = aweights, aec = aec))

  # Create a list N deep, each item itself a list of the three xts.
  # Refer to each xts as:  eclist$ec1$arets  or eclist[['ec1']][['arets']]
  eclist  <- rep(single_list, N)
  names(eclist) <- ecnames


  #-----------------------------------------------------------------
  # MAIN loop - over N portfolios:
  #   . Populate the arets matrix using K samples (without
  #     replacement for each data row).
  #   . Populate the aweights matrix
  #   . Calculate the aec equity curve
  #
  # Repeat the code for each of value of weights (uniform or equal)
  # to maximize speed by taking the if out of the loop.
  #
  # This code is somewhat slow but using lapply isn't faster,
  # and the byte compiler doesn't buy much at all.
  # Stick with the for-loop for now (easier to understand).
  #----------------------------------------------------------------
  if(weights == "equal") {
    for(i in 1:N) {
      eclist[[i]][['arets']] <- t(apply(data, 1, function(x)
        sample(x, size = K, replace = FALSE)))

      # We already have equal weights in the set of
      # aweights matrices, so no need to repeat it here.

      # returns * weights
      eclist[[i]][['aec']] <- eclist[[i]][['arets']] * aweights

      # equity curve expressed as returns
      pecr[, i]            <- apply(eclist[[i]][['aec']], 1, sum)

    }

  } else {
    # weights == "uniform"
    for(i in 1:N) {
      eclist[[i]][['arets']] <- t(apply(data, 1, function(x)
        sample(x, size = K, replace = FALSE)))

      # uniform random weights, normalized to sum to one.
      aweights[] <- runif(length(aweights), min = 0, max = 1)
      aweights[] <- aweights / apply(aweights, 1, sum)
      eclist[[i]][['aweights']] <- aweights

      # returns * weights
      eclist[[i]][['aec']] <- eclist[[i]][['arets']] * aweights

      # equity curve expressed as returns
      pecr[, i]            <- apply(eclist[[i]][['aec']], 1, sum)

    }

  }   ###### END IF statement  ######




  # These functions are very fast!
  ec          <- cumprod_na(1 + pecr) # very fast!
  cagr        <- as.numeric(xtscagr(ec))
  names(cagr) <- colnames(ec)

  allmdd        <- as.numeric(xtsmdd(pecr))
  names(allmdd) <- colnames(ec)
  allmar        <- cagr / -allmdd

  cagr_vec <- c(mean(cagr),   sd(cagr),   skewness(cagr),   kurtosis(cagr))
  mdd_vec  <- c(mean(allmdd), sd(allmdd), skewness(allmdd), kurtosis(allmdd))
  mar_vec  <- c(mean(allmar), sd(allmar), skewness(allmar), kurtosis(allmar))

  vecnames <- c("Mean", "SDev", "Skewness", "Kurtosis")
  names(cagr_vec) <- vecnames
  names(mdd_vec)  <- vecnames
  names(mar_vec)  <- vecnames

  retlist <- list(cagr      = cagr_vec,
                  mdd       = mdd_vec,
                  mar       = mar_vec,
                  cagr_dist = cagr,
                  mdd_dist  = allmdd,
                  mar_dist  = allmar)

  return(retlist)

}



# #
# #
# templist  <- as.list(rep(NA, N))
# names(templist) <- ecnames
# #
# # # use lapply...
#  microbenchmark(templist <- lapply(templist, function(y) {
#    t(apply(data, 1, function(x) sample(x, size = K, replace = FALSE)))
#  }), times = 5 )
#
# # compiled version
# fn1 <- function(templist, data, K) {
#   x <- lapply(templist, function(y) t(apply(data, 1, function(x)
#     sample(x, size = K, replace = FALSE))))
# }
#
# fn1c <- compiler::cmpfun(fn1)
#
# xx  <- fn1(templist = templist, data = data, K = K)
# xxc <- fn1c(templist = templist, data = data, K = K)
#
# res <- microbenchmark(fn1(templist, data, K),
#                       fn1c(templist, data, K),
#                       times = 5)
#
#

