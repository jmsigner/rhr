##' Isopleths of Home Range Estimate
##'
##' Function to retrieve isopleths of a home range estimate. 
##'
##' Probabilistic estimators take (i.e. kernel density estimates) take
##' an additional argument, \code{levels}, that determines which isopleth are
##' returned.
##'
##' @template RhrEst
##' @param ... see details.
##' @return \code{SpatialPolygonsDataFrame} 
##' @export
##' @example inst/examples/rhrIsopleths.R

rhrIsopleths <- function (x, ...) {
  UseMethod ("rhrIsopleths", x)
}

##' @export
rhrIsopleths.default <- function (x , ...) {
  paste0 ("rhrIsopleths is not defined for object of class", class(x))
}
