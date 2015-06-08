##' Retrieve relocations of a trajectory
##'
##' Retruns the relocation of a trajectory as \code{SpatialPoints}.
##'
##' @param x Object of class \code{RhrTrajS*}
##' @return Object of class \code{SpatialPoints}
##' @export

rhrTrajRelocations <- function(x, ...) {
  UseMethod("rhrTrajRelocations")
}

##' @export
rhrTrajRelocations.RhrTrajS <- function(x) {
  x[["traj"]]
}

##' @export
rhrTrajRelocations.RhrTrajST <- function(x) {
  SpatialPointsDataFrame(slot(x[["traj"]], "sp"), 
                         slot(x[["traj"]], "data"))
}

##' @export
rhrTrajRelocations.RhrTrajSTR <- function(x) {
  sp <- SpatialPointsDataFrame(slot(x[["traj"]], "sp"), slot(x[["traj"]], "data"))
  attr <- x[["attribute"]] 
  suppressWarnings(sp::merge(sp, attr, by = "rhrTrajTime"))
}
