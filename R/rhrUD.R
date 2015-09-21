#' Utilization distribution
#'
#' Function to obtain the utilization distribution (UD) of a home range estimate.
#' 
#' For probabilistic estimators home range estimator this functions returns the UD.
#' @param x An object of class RhrProbEst.
#' @param ... none implemented.
#' @export
#' @return An object of class RasterLayer.
#' @example inst/examples/rhrUD.R

rhrUD <- function(x, ...) {
  UseMethod("rhrUD", x)
}

#' @export
rhrUD.default <- function (x , ...) {
  paste0 ("rhrUD is not defined for object of class", class(x))
}

##' @export
rhrUD.RhrGeoEst <- function (x , ...) {
  warning("rhrUD is not defined for geometric estimators")
}
