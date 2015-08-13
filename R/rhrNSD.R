#' Net squared deplacement
#' 
#' Calcualtes the net squuared deplacement from the first point.
#' 
#' @param x RhrTrack* object 
#' 
#' @examples 
#' data(trackS)
#' traj <- rhrTraj(SpatialPoints(datSH[, 2:3]))
#' plot(rhrNSD(traj), type= "l")
#' @export
rhrNSD <- function(x, ...) {
  UseMethod("rhrNSD")
}

#' @export
rhrNSD.RhrTrackS <- function(x, ...) {
  xx <- coordinates(rhrPoints(x))
  sp::spDistsN1(as.matrix(xx), 
                as.numeric(xx[1, ]))
}
