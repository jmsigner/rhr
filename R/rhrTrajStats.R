##' Retunrs trajectory statistics
##'
##' Retunrs trajectory statistics
##'
##' @param x Object of class \code{RhrTrajS*}
##' @return Object of class \code{data.frame}
##' @export
rhrTrackStats <- function(x, ...) {
  UseMethod("rhrTrackStats")
}

#' @export
rhrTrackStats.RhrTraj <- function(x, ...) {
  if (is.null(x[["segments"]])) {
    x[["segments"]] <- rhrCalcTrackStats(x)
  }
  x[["segments"]]
}
