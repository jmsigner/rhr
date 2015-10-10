#' Print home-range areas in a pretty way
#'
#' Creates a table of home-range areas and optionally converts units.
#' 
#' @param x An object of class RhrEst*.
#' @param inU Character scalar specifying input units.
#' @param outU Character scalar specifying output units.
#' @param ... none implemented.
#' @export
#' @return A data.frame.
#' @seealso \code{rhrConvertUnit}

rhrPrettyArea <- function(x, inU, outU, ...) {
  UseMethod("rhrPrettyArea", x)
}

#' @export
rhrPrettyArea.RhrEst <- function(x, inU = "ido", outU = "ius", ...) {
  ar <- rhrArea(x)
  ar$area <- rhrConvertUnit(ar$area, inUnit = inU, outUnit = outU)
  ar
}


#' @export
rhrPrettyArea.default <- function (x , ...) {
  paste0 ("rhrPrettyArea is not defined for object of class", class(x))
}
