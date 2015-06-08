##' Gaps of a trajectory
##'
##' Extracts gaps of a regular trajectory (i.e., time-stamps with missing spatial information).
##'
##' @param x Object of class \code{RhrTrajSTR}
##' @export

rhrTrajGaps <- function(x, ...) {
  UseMethod("rhrTrajGaps", x)
}

##' @export
rhrTrajGaps.RhrTrajSTR <- function(x) {
  xx <- rhrTrajAttribute(x)
  xx <- xx[!xx$rhrTrajSpace, ]
  xx$rhrTrajTime
}
