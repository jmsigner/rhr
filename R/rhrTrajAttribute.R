##' Retunrs trajectory attributes
##'
##' Retunrs trajectory attributes
##'
##' @param x Object of class \code{RhrTrajS*}
##' @return Object of class \code{data.frame}
##' @export
rhrTrajAttribute <- function(x, ...) {
  UseMethod("rhrTrajAttribute", x)
}

##' @export
rhrTrajAttribute.RhrTraj <- function(x) {
  slot(x[["traj"]], "data") 
}

##' @export
rhrTrajAttribute.RhrTrajSTR <- function(x) {
  x[["attribute"]]
}
