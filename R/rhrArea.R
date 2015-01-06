##' Returns the area of RhrEst object
##'
##' @title rhrArea
##' @param x object of class RhrEst
##' @param ... none implemented
##' @return vector
##' @export
##' @author Johannes Signer
rhrArea <- function (x , ...) {
  UseMethod ("rhrArea", x)
}

##' @export
rhrArea.default <- function (x , ...) {
  paste0 ("rhrArea is not defined for object of class ", class(x))
}
