#' Functions to work with results of a home range estimate.
#' 
#' This collection is a set of utility function to work with results of a home range estimate. \code{rhrArgs} returns the arguments that where passed to a call.
#' 
#' More to do here
#'
#' Returns a list of all arguments that were passed to an \code{rhr*} function call
#' 
#' @param x An object of class RhrEst*.
#' @template dots
#' @return A list.
#' @name hr_utility
#' @examples
#' data(datSH)
#' mcp <- rhrMCP(datSH[, 2:3])
#' str(rhrArgs(mcp))
NULL

#' @export
#' @rdname hr_utility
#' 
rhrArgs <- function(x, ...) {
  UseMethod("rhrArgs", x)
}

#' @export
rhrArgs.default <- function (x , ...) {
  paste0 ("rhrArgs is not defined for object of class", class(x))
}

