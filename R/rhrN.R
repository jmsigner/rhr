##' Number of relocations in a trajectory
##'
##' Retunrs the number of relocations in a trajectory
##'
##' @param x Object of class \code{RhrTrajS*}
##' @param missing logical scalar, should the number of missing locations be returned?
##' @export
rhrN <- function(x, ...) {
  UseMethod("rhrN", x)
}

##' @export
rhrN.RhrTraj <- function(x, ...) {
  length(rhrTrajLocations(x))
}

##' @export
rhrN.RhrTrajSTR <- function(x, missing = FALSE, ...) {
  NROW(x[["trajFull"]]) - if (missing) length(rhrTrajRelocations(x)) else 0
}
