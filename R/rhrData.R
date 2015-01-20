##' Return Data
##'
##' Function to retrieve data that were used generate the estimate.
##'
##' @template RhrEst
##' @param ... none implemented.
##' @param spatial Logical value indicating whether or not to return \code{SpatialPoints}.
##' @return Depending on the argument, either a \code{data.frame} or \code{SpatialPointsDataFrame}.
##' @export
##' @example inst/examples/rhrData.R

rhrData <- function (x, spatial=FALSE, ...) {
  UseMethod ("rhrData", x)
}

##' @export
rhrData.default <- function (x, spatial=FALSE, ...) {
  paste0 ("rhrData is not defined for object of class ", class(x))
}
