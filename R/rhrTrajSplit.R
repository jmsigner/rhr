#' Split trajectory
#' 
#' Split a trajectory into a list of new trajectories. 
#' 
#' @param x RhrTraj* object.
#' @param f factor that provides a grouping for splitting data.
#' @export
rhrTrajSplit <- function(x, f, ...) {
  UseMethod("rhrTrajSplit")
}

#' @export
rhrTrajSplit.RhrTraj <- function(x, f, ...) {
  x <- rhrTrajLocations(x)
  lapply(split(x, f), rhrTrajConstructor)
}
  
#' @export
rhrTrajSplit.RhrTrajSTR <- function(x, f, ...) {
  lapply(split(1:nrow(x), f), function(i) {
    xx <- blowupTraj(x[i, ])
    rhrTrajConstructorSTR(xx[[1]], xx[[2]])
  })
}
