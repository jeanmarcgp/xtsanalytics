% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/genpurpose.R
\name{sprint}
\alias{sprint}
\alias{sprintcat}
\title{Print to console using C-like sprintf formatting.}
\usage{
sprint(string, ...)

sprintcat(string, ...)
}
\arguments{
\item{string}{A character string containing text and formatting style.}

\item{...}{Additional parameters passed to sprintf.  In most cases, these
are the sequence of variables that are printed, provided they
have an associated formatting in the string.}
}
\value{
No return value.  This function is called for its side effects
        by printing on the console.
}
\description{
A simple function to print text and variable content on the console, using
the C-like function sprintf formatting style.
}
\details{
There are two versions of this function:  sprint and sprintcat.
Function sprint automatically appends a carriage return at the end of
the string whereas sprintcat does not.
}
\section{Functions}{
\itemize{
\item \code{sprintcat}: Similar to sprint but without the carriage return
}}

\seealso{
sprintf()
}
