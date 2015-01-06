##' Returns the area of RhrEst object
##'
##' @title rhrData
##' @param x object of class RhrEst
##' @param spatial logical value indicating whether or not to return spatial points
##' @param ... not implemented
##' @return data.frame
##' @export
##' @author Johannes Signer
rhrData <- function (x, spatial=FALSE, ...) {
  UseMethod ("rhrData", x)
}

##' @export
rhrData.default <- function (x, spatial=FALSE, ...) {
  paste0 ("rhrData is not defined for object of class ", class(x))
}
