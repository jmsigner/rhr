##' Returns the levels of RhrEst object
##'
##' @title rhrLevels
##' @param x object of class RhrEst
##' @param ... 
##' @return vector
##' @export
##' @author Johannes Signer
rhrLevels <- function (x , ...) {
  UseMethod ("rhrLevels", x)
}

##' @export
rhrLevels.default <- function (x , ...) {
  paste0 ("rhrLevels is not defined for object of class ", class(x))
}

##' @export
rhrLevels.RhrProbEst <- function (x , ...) {
  message("Probabilistic estimators: to get home range at specific level call 'rhrArea(est, level=95)', or 'rhrIsopleths(est, level=95)' to get contour lines.")
}
