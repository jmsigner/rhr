##' Level(s) of Home Range Estimate
##'
##' Function to retrieve the levels of a home range estimate. 
##'
##' @template RhrEst
##' @param ... none implemented.
##' @return Numeric \code{vector} with the levels.
##' @export
##' @example inst/examples/rhrLevels.R

rhrLevels <- function (x , ...) {
  UseMethod ("rhrLevels", x)
}

##' @export
rhrLevels.default <- function (x , ...) {
  paste0 ("rhrLevels is not defined for object of class ", class(x))
}

##' @export
rhrLevels.RhrProbEst <- function (x , ...) {
  if (is.null(x$args$levels)) {
    message("Probabilistic estimators: to get home range at specific level call 'rhrArea(est, level=95)', or 'rhrIsopleths(est, level=95)' to get contour lines.")
  } else {
    x$args$levels
  }
}
