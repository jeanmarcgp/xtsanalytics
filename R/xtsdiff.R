#------------------------------------------------------------------------------------
#  xtsdiff.R
#  ---------
#
#  FUNCTION xtsdiff
#
#' Normalize a time series and compute its difference to a benchmark
#'
#' This function is typically used to calculate the efficiency of
#' inverse funds or ETFs with respect to a benchmark such as its underlying
#' index.
#'
#' An xts time series is provided.  Its returns
#' are computed and multiplied by a constant k to make it equivalent to the
#' benchmark.  A list of three quantities are retured:  the returns series ($rets),
#' a normalized equity curve ($ec), and the percentage difference between the
#' equity curves ($diff).
#'
#' If more than 2 time series are provided (more than 2 columns), all series
#' including the benchmark are implicitly truncated to align at the start of
#' the latest series. This is so the equity curve of the benchmark and all
#' others start at the same time.
#'
#' @param data    The xts time series to normalize
#'
#' @param k       The constant (a vector) used to multiply the returns
#'                of the time series. The length of this vector must equal
#'                the number of columns in the xts matrix provided.
#'
#' @param bench   Name of the benchmark from which to compute the differences.
#'                If NA (default), then $diff is not computed and not returned
#'                in the return list.
#'
#' @param na.omit Logical. Specifies whether NA rows are omitted.  This effectively
#'                removes all leading rows containing NAs.
#'
#' @return A list containting three xts matrices:  $rets contains a matrix of
#'         returns, $ec contains a matrix of equity curves, and $diff contains
#'         a matrix of differences.
#'
#' @export
#------------------------------------------------------------------------------------
xtsdiff <- function(data, k = -1, bench = NA, na.omit = TRUE) {

  # In case k is a vector or data contains multiple columns
  if(ncol(data) != length(k))
    stop("xtsnormalize:  length(k) must equal ncol(data).")

  rets   <- xts(ROC(data, type="discrete") %*% diag(k), order.by = index(data))

  colnames(rets) <- paste0(colnames(data), "_rets")

  if(na.omit) rets<- rets[complete.cases(rets), ]

  ec <- cumprod_na(1 + rets)
  colnames(ec) <- paste0(colnames(data), "_ec")
  ec[is.na(rets)] <- NA    # Substitute leading 1's for NAs in equity curve

  # for each equity curve other than bench, compute an equivalent
  # benchmark equity curve and then subtract if from the ec to get a difference.
  # perhaps add $bench_ec in retval which is the equity curve of the benchmark
  # normalized at the same point as the other equity curves?

  retval <- list(rets = rets, ec = ec)
  if(!is.na(bench)) {
    if(!(bench %in% colnames(data) || as.numeric(bench) <= length(colnames(data))))
      stop("xtsdiff:  bench must be a valid column name.")


    if(is.numeric(bench)) benchnum <- bench else
      benchnum <- match(bench, colnames(data))

    sprint("bench is: %s, index in data is: %s", bench, benchnum)
    benchvec <- as.numeric(ec[, benchnum])
    ecdiff <- as.xts(apply(ec, 2, function(x) { (benchvec - x) / benchvec }))
    retval <- c(retval, list(diff = ecdiff))

  }


  return(retval)

}
