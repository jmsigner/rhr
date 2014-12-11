##' Generic Function to check if an ud is present
##' 
##' @param x an object of class RhrEst
##' @param ... further arguments, none implemented
##' @export
##' @rdname rhrHasUD
##' @return logical value indicating whether or not a UD is present

rhrHasUD <- function(x, ...) {
  UseMethod("rhrHasUD", x)
}

##' @export
##' @rdname rhrHasUD
rhrHasUD.RhrGeoEst <- function(x, ...) {
  FALSE
}


