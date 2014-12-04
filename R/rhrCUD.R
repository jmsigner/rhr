##' Generic Function to calculate cumulative UD
##' 
##' @param x an object of class RhrHrEstimator*
##' @param ... further arguments, none implemented
##' @export
##' @return an object of class raster

rhrCUD <- function(x, ...) {
  UseMethod("rhrCUD", x)
}

##' @export
rhrCUD.default <- function (x , ...) {
  paste0("rhrCUD is not defined for object of class ", class(x))
}

##' @export
rhrCUD.RhrGeoEst <- function (x , ...) {
  warning("rhrCUD is not defined for geometric estimators")
}
