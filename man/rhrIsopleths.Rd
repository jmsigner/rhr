% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rhrIsopleths.R
\name{rhrIsopleths}
\alias{rhrIsopleths}
\title{Isopleths of Home Range Estimate}
\usage{
rhrIsopleths(x, ...)
}
\arguments{
\item{x}{Instance of \code{RhrEst}.}

\item{...}{see details.}
}
\value{
\code{SpatialPolygonsDataFrame}
}
\description{
Function to retrieve isopleths of a home range estimate.
}
\details{
Probabilistic estimators take (i.e. kernel density estimates) take
an additional argument, \code{levels}, that determines which isopleth are
returned.
}

