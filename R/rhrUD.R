##' Generic Function to calculate UD
##' 
##' @param x an object of class RhrHrEstimator*
##' @param ... further arguments, none implemented
##' @export
##' @return an object of class raster

rhrUD <- function(x, ...) {
  UseMethod("rhrUD", x)
}

##' @export
rhrUD.default <- function (x , ...) {
  paste0 ("rhrUD is not defined for object of class", class(x))
}

##' @export
rhrUD.RhrGeoEst <- function (x , ...) {
  warning("rhrUD is not defined for geometric estimators")
}
