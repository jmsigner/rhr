##' Checks if trajectory has a time stamp
##'
##' Retunrs TRUE if the trajectory has a time stamp and FALSE if not. 
##'
##' @param x Object of class \code{RhrTrajS*}
##' @export
rhrHasTS <- function(x, ...) {
UseMethod("rhrHasTS", x)
}

##' @export
rhrHasTS.RhrTraj <- function(x, ...) {
  is(x, "RhrTrajST")
}
