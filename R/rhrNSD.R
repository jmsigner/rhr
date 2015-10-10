#' Net squared deplacement
#'
#' Calcualtes the net squuared deplacement from the first point.
#'
#' @param x RhrTrack* object
#' @param ... None implemented.
#'
#' @export
rhrNSD <- function(x, ...) {
  UseMethod("rhrNSD")
}

#' @export
rhrNSD.RhrTrackS <- function(x, ...) {
  xx <- sp::coordinates(rhrPoints(x))
  sp::spDistsN1(as.matrix(xx),
                as.numeric(xx[1, ]))^2
}
