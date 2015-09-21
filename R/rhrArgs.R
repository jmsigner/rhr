#' Arguments passed to a function call
#'
#' Returns a list of all arguments that were passed to an \code{rhr*} function call
#' 
#' @param x An object of class RhrEst*.
#' @param ... none implemented.
#' @export
#' @return A list.
#' @example inst/examples/ex-rhrArgs.R

rhrArgs <- function(x, ...) {
  UseMethod("rhrArgs", x)
}

#' @export
rhrArgs.default <- function (x , ...) {
  paste0 ("rhrArgs is not defined for object of class", class(x))
}

