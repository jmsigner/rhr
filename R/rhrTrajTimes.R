##' Time of relocations
##'
##' Retunrs the time a relocation was recorded
##'
##' @param x Object of class \code{RhrTrajS*}
##' @param missing logical scalar, should the number of missing locations be returned?
##' @export
##' 
rhrTrajTimes <- function(x, ...) {
  UseMethod("rhrTrajTimes", x)
}

##' @export
rhrTrajTimes.RhrTrajST <- function(x, ...) {
  time(rhrTrajLocations(x)@time)
}

##' @export
rhrTrajTimes.RhrTrajSTR <- function(x, ...) {
  prop <- x[["properties"]]
  seq(prop$from, prop$to, paste(prop$by, "sec"))
}
