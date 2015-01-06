##' Returns the isopleths of RhrEst object
##'
##' @title rhrIsopleths
##' @param x object of class RhrEst
##' @param ... not implemented 
##' @return SpatialPolygonsDataFrame
##' @export
##' @author Johannes Signer
rhrIsopleths <- function (x, ...) {
  UseMethod ("rhrIsopleths", x)
}

##' @export
rhrIsopleths.default <- function (x , ...) {
  paste0 ("rhrIsopleths is not defined for object of class", class(x))
}
