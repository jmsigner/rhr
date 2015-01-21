##' Tuning parameter of Home Range Estimate
##'
##' Function to retrieve the tuning parameter(s) of a home range estimate. 
##'
##' Some home range estimators require a tuning parameter for the estimation
##' (e.g. the bandwidth for kernel density estimates). This functions retrieves
##' the value of the tuning parameter.
##' If \code{msg = TRUE} values are rounded to the \code{digit} decimal place.
##'
##' @template RhrEst
##' @param ... none implemented
##' @param msg Boolean scalar, indicating if the answer should be returned as message.
##' @param digits Numeric scalar, giving the number of decimal places to round to.
##' @return If \code{msg} is \code{TRUE} a character string, else a list with
##' the name of the tuning parameter and its value.
##' @export
##' @example inst/examples/rhrTuningParameter.R

rhrTuningParameter <- function (x, msg = FALSE, digits = 3, ...) {
  UseMethod ("rhrTuningParameter", x)
}

##' @export
rhrTuningParameter.default <- function (x , ...) {
  paste0 ("rhrTuningParameter is not defined for object of class ", class(x))
}
