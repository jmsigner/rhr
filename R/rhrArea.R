##' Area of Home Range Estimate
##'
##' Function to calculate the area of a home range estimate. 
##'
##' For geometric estimators the isopleth levels at which the area is calculated is
##' already determined when the estimate is performed. Probabilistic estimators take
##' an additional argument, \code{levels}, that determines at which isopleth levels
##' of the UD the area is evaluated. 
##'
##' @template RhrEst
##' @param ... see details.
##' @return \code{data.frame} with the isopleth level and area in units of the coordinate system. 
##' @export
##' @example inst/examples/rhrArea.R

rhrArea <- function (x , ...) {
  UseMethod ("rhrArea", x)
}

##' @export
rhrArea.default <- function (x , ...) {
  paste0 ("rhrArea is not defined for object of class ", class(x))
}
