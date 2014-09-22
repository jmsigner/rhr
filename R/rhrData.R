##' Returns the area of RhrEst object
##'
##' @title rhrData
##' @param obj object of class RhrEst
##' @param ... 
##' @return data.frame
##' @export
##' @author Johannes Signer
rhrData <- function (x , ...) {
  UseMethod ("rhrData", x)
}

##' @export
rhrData.default <- function (x , ...) {
  paste0 ("rhrData is not defined for object of class ", class(x))
}
