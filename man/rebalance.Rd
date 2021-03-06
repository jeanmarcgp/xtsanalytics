% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rebalance.R
\name{rebalance}
\alias{rebalance}
\title{Periodic portfolio rebalance to a given set of weights}
\usage{
rebalance(prices, weights, on = "months", rebal_offset = 1)
}
\arguments{
\item{prices}{An xts of asset prices from which to build the equity curve
using periodic rebalancing.}

\item{weights}{A list or vector of asset symbols with their associated weights.
This vector should normally sum up to one.  However, what matters
is the relative ratios between each weight, not the absolute values.
The list or vector must be named, and each name must have a
corresponding column in the prices xts matrix.}

\item{on}{Period on which to rebalance using function endpoints.  Valid values are:
{ 'weeks', 'months', 'quarters', 'years' }.}

\item{rebal_offset}{Number of days from which to offset the rebalance.  Default is one,
which corresponds to the day after the period endpoints.  Can only be zero or
a positive number (which rebalances later than the endpoint).}
}
\value{
An xts matrix of the portfolio equity curve, rebalanced at the end of the
        specified trading day by argument on.  The first column contains the prices
        equity curve while the second column contains the daily returns of the equity
        curve.
}
\description{
Periodically rebalances a portfolio to a set of weights according to a weight vector.
}
