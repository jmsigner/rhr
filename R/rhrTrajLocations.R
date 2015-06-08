##' Retrieves the locations of a  trajectory
##'
##' Retruns the location of a trajectory as \code{sp::SpatialPoints} for trajectories without temporal information and as \code{spacetime::STIDF} for trajectories with temporal information.
##'
##' @param x Object of class \code{RhrTrajS*}
##' @return Object of class \code{SpatialPoints} or \code{STIDF}
##' @export

#' @export
rhrTrajLocations <- function(x, ...) {
  UseMethod("rhrTrajLocations")
}

#' @export
rhrTrajLocations.RhrTraj <- function(x) {
  x[["traj"]]
}
