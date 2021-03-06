% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rhrLoCoH.R
\name{rhrLoCoH}
\alias{rhrLoCoH}
\title{Local Convex Hull (LoCoH)}
\usage{
rhrLoCoH(xy, type = "k", n = 10, levels = 95, minPts = 3,
  autoN = FALSE)
}
\arguments{
\item{xy}{Instance of \code{data.frame}, \code{SpatialPoints},
\code{SpatialPointsDataFrame} or \code{RhrMappedData}. If \code{xy} is
is a \code{data.frame}, then the first two columns are assumed to be the
\code{x} and \code{y} coordinates of the relocations.}

\item{type}{Scalar character, one of "k", "r", "a". Method to determine the tuning parameter \code{n}.}

\item{n}{Numeric scalar, value of the tuning parameter.}

\item{levels}{Numeric \code{vector} indicating percentages at which the home-range estimates should be evaluated.}

\item{minPts}{Numeric scalar, the minimum number of neighbors required.}

\item{autoN}{Boolean scalar, whether or not \code{n} should be determined automatically.}
}
\value{
Object of class \code{RhrLoCoH}
}
\description{
Function to estimate home ranges with local convex hulls.
}
\details{
Three different methods to determine the neighboring points are available:
\itemize{
 \item{"k"}{uses k-nearest neighbors}
 \item{"r"}{uses all neighbors within a radius r}
 \item{"a"}{uses all neighbors that can be reached within a distance a. The distance to all points is calculated and then cummulatively summed starting from the smallest until \code{a} is reached.}}.
\code{autoN} attempts to estimate values for the tuning parameters k,r,a.
If \code{type} is \code{"k"} then \code{"n"} is the square root of all locations.
If \code{type} is \code{"a"} then \code{"n"} is the maximum distance between any two relocations.
If \code{type} is \code{"r"} then \code{"n"} is 5 % of the maximum distance between any two relocations.
}
\references{
Getz, W. M., & Wilmers, C. C. (2004). A local nearest-neighbor convex-hull construction of home ranges and utilization distributions. _Ecography_, 27(4), 489-505.

Getz, W. M., Fortmann-Roe, S., Cross, P. C., Lyons, A. J., Ryan, S. J., & Wilmers, C. C. (2007). LoCoH: nonparameteric kernel methods for constructing home ranges and utilization distributions. _PloS one_, 2(2), e207.
}

