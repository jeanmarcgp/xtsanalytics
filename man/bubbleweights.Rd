% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bubbleweights.R
\name{bubbleweights}
\alias{bubbleweights}
\title{Generate a bubble plot of asset weights.}
\usage{
bubbleweights(x, y, dotweights, dotclass = NA, labelpos = NA,
  xlab = "Volatility (\%)", ylab = "Momentum (\%)",
  legendloc = "topleft", cex.dots = 0.8, cex.legend = 0.8,
  hline = NA, main = "Momentum vs. Volatility Bubble Plot", ...)
}
\arguments{
\item{x}{A named vector or single row xts containing the X values.  If a
single row xts, the index is ignored, but the column names are
used to name any unnamed vector provided for y or dotweights.}

\item{y}{An order-matched unnamed vector, or a single row xts containing
the Y values.}

\item{dotweights}{An order-matched vector or single row xts containing the relative weights
of each dots.}

\item{dotclass}{A list associating each asset to its asset class.  The list follows the
format asset_name = "asset class" e.g. SPY = "equities".  Up to TBD asset
classes can be named with colors automatically assigned.}

\item{labelpos}{A named vector or a list of labels associating some or all assets
to a non-default location relative to the dot on the plot.  The default
label location is on the right.  Valid locations are
"above", "below", "left" or "right".  Format for the list is:
SPY = "above". Default is NA i.e. no exceptions, all labels on the right.}

\item{xlab}{The x-axis label.}

\item{ylab}{The y-axis label.}

\item{legendloc}{The legend location.  Valid values are "bottomright", "bottom",
"bottomleft", "left", "topleft", "top", "topright", "right" and "center".}

\item{cex.dots}{Relative scaling of the labels on each dot.}

\item{cex.legend}{Relative scaling of the legend.}

\item{hline}{The vertical location to place a horizontal line.  Default is NA (no line).}

\item{main}{The plot title.  The index(last(x)) is automatically appended
and plotted under the title to show the date of the analysis.}

\item{...}{Additional parameters passed through to function plot.}
}
\description{
This function is useful to visualize the momentm vs. volatility of a universe
of assets, along with the relative weights of each asset.  The colors of each
bubble dot is chosen based on the asset class for the given dot.
}
