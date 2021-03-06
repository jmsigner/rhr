% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rhrHrefRescaled.R
\name{rhrHrefScaled}
\alias{rhrHrefScaled}
\title{Select a bandwidth for Kernel Density Estimation}
\usage{
rhrHrefScaled(xy, range = rhrHref(xy)$h * c(0.01, 1),
  trast = rhrRasterFromExt(rhrExtFromPoints(xy, extendRange = 0.2), nrow =
  100, res = NULL), numOfParts = 1, levels = 95, tol = 0.1, maxIt = 500)
}
\arguments{
\item{xy}{Instance of \code{data.frame}, \code{SpatialPoints},
\code{SpatialPointsDataFrame} or \code{RhrMappedData}. If \code{xy} is
is a \code{data.frame}, then the first two columns are assumed to be the
\code{x} and \code{y} coordinates of the relocations.}

\item{range}{Numeric vector, indicating the lower and upper bound of the search range. If \code{range} is to large with regard to \code{trast}, the algorithm will fail.}

\item{trast}{Instance of \code{RasterLayer} that serves as a template for the resulting grid of the estimation.}

\item{numOfParts}{Numeric numeric scalar, indicating the number of contiguous  polygons desired. This will usually be one.}

\item{levels}{Numeric \code{vector} indicating percentages at which the home-range estimates should be evaluated.}

\item{tol}{Numeric scalar, indicating which difference of to stop.}

\item{maxIt}{Numeric scalar, indicating the maximum number of acceptable iterations.}
}
\value{
\code{list} with the calculated bandwidth, exit status and the number of iteration.
}
\description{
Use two dimensional reference bandwidth to select a bandwidth for kernel density estimation. 
Find the smallest value for bandwidth (h) that results in n polygons
(usually n=1) contiguous polygons at a given level.
}
\details{
This implementation uses a bisection algorithm to the find the smallest value
value for the kernel bandwidth within \code{range} that produces an home-range
isopleth at \code{level} consisting of \code{n} polygons. Note, no difference is
is made between the two dimensions.
}
\references{
Kie, John G. "A rule-based ad hoc method for selecting a bandwidth in kernel home-range analyses." Animal Biotelemetry 1.1 (2013): 1-12.
}

