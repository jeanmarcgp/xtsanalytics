% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rebalance.R
\name{rebalance}
\alias{rebalance}
\title{Periodic portfolio rebalance to a given set of weights (NEED TO TEST)}
\usage{
rebalance(prices, weights, on = "quarters", offset = 0, normalize = TRUE)
}
\arguments{
\item{prices}{An xts of asset prices from which to build the equity curve
using rebalancing.}

\item{weights}{A list or vector of asset symbols with their associated weights.
This vector must sum up to one.}

\item{on}{Period on which to rebalance using function endpoints.  Valid values are:
{ 'weeks', 'months', 'quarters', 'years' }.  If on is a vector, then
multiple equity curves are produced corresponding to each value of on.}

\item{offset}{Number of days from which to offset the rebalance.  Default is zero,
which corresponds to the on period endpoints.  May be a positive or
negative number.}

\item{normalize}{Logical.  Normalizes the equity curve of each asset price before
doing a rebalance.}
}
\value{
An xts matrix of the portfolio equity curve, rebalanced at the end of the
        specified trading day by argument on.  If argument on is a vector then there
        will be length(on) equity curves produced.
}
\description{
Periodically rebalances a portfolio to a set of weights according to a weight vector.
}
\details{
NOTE:  This may be buggy!  Test and examine the code first!!!
}
