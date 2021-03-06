% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/roll_bootstrap.R
\name{roll_bootstrap}
\alias{roll_bootstrap}
\title{Generates a rolling window bootstrap index vector to synthesize a random series}
\usage{
roll_bootstrap(N, k = 3, nc = 1)
}
\arguments{
\item{N}{The length of the vector we wish to generate}

\item{k}{The rolling window width}

\item{nc}{The number of columns to generate (number of vectors)}
}
\value{
Returns a numeric matrix of N rows by nc columns. Consider using
        apply to then apply these index to generate an equivalent xts matrix.
        See examples to understand how to do this.
}
\description{
This function generates an index vector that can be used to
subset an xts matrix in a rolling window random fashion.
For speed, the underlying code uses Rcpp to generate the random
vectors.
}
\details{
The function is used to create synthetic time series by reordering
the returns randomly in a rolling window fashion.

The implication is any autocorrelation between consecutive or near
consecutive return samples will be randomly shuffled.  However, the
heteroscedastic properties of the series over longer time frames is
mostly maintained.
}
