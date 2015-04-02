#' Interval Check
#'
#' Checks if consecutive observations are separated by an interval or not. If \code{int} is used up, than the next observation is used.
#'
#' @param ts Numeric vector, the time stamps in seconds.
#' @param int Numeric scalar, the minimum time in seconds that two observations need to be separated.
#' @export
#' @import Rcpp
#' @return A logcial vector of length \code{length(ts)}, indicating if a given observation is seperated from the last chosen one by int.
#' @example inst/examples/rhrBaseIntervalSubset.R


rhrBaseIntervalSubset <- function(ts, int) {

  if (length(ts) < 2) {
    stop("length(ts) < 2")
  }

  if (!is.numeric(ts) | !is.numeric(int)) {
    stop("ts and int are required to be numeric")
  }

  as.logical(t2cpp3(ts, int))
}


#' Least Square Cross Validation
#'
#' Least square cross validation to determine bandwidth for  kernel density estimation.
#'
#' @param x Numeric vector, the x coordinates.
#' @param y Numeric vector, the y cooridnates.
#' @param h Numeric vector, candidate bandwidth.
#' @export
#' @import Rcpp
#' @useDynLib rhrBase
#' @return Numeric vector, the score value for each of the proposed candidate bandwidths.
#' @references \url{http://www.esajournals.org/doi/abs/10.2307/2265701}
#'
#' @example inst/examples/rhrBaseLSCV.R


rhrBaseLSCV <- function(x, y, h) {

  if (length(x) != length(y)) {
    stop("x and y are required to be of the same length")
  }

  if (!is.numeric(x) | !is.numeric(y) | !is.numeric(h)) {
    stop("x, y and h are required to be numeric")
  }
  unbinnedCV(x, y, h)
}


#' Mean squared distance
#'
#' Mean squared distance from the center of activity.
#'
#' @param x Numeric vector, the x cooridnates
#' @param y Numeric vector, the y coordinates
#' @return Numeric matrix with the x and y coordinates of the new path.
#' @references \url{http://www.esajournals.org/doi/abs/10.2307/1937590}

#' @export
#' @example inst/examples/rhrBaseMSD.R

rhrBaseMSD <- function(x, y) {

  if (length(x) != length(y)) {
    stop("x and y are not of the same length")
  }

  if (!is.numeric(x) | !is.numeric(y)) {
    stop("x and y are required to be numeric")
  }

  mx  <- mean(x)
  my  <- mean(y)
  meanSquaredDistance(x, y, mx, my)
}


#' Permutate walk
#'
#' permuates a walk by reusing the original distances between steps and connecting them with angles drawn from a uniform distribution between 0 and 360.
#' @param x Numeric vector, the x coordinates.
#' @param y Numeric vector, the y coordinates.
#' @return Numeric matrix with the x and y coordinates of the new path.
#' @references \url{http://www.esajournals.org/doi/abs/10.2307/1939170}

#' @export
#' @example inst/examples/rhrBasePRW.R

rhrBasePRW <- function(x, y) {

  if (length(x) != length(y)) {
    stop("x and y are not of the same length")
  }

  if (!is.numeric(x) | !is.numeric(y)) {
    stop("x and y are required to be numeric")
  }

  d <- sqrt((x[-1] - x[-length(x)])^2 + (y[-1] - y[-length(y)])^2)
  d <- sample(d)

  rA <- runif(length(d), 0, 360)

  sinrA <- sin(rA * pi/180)
  cosrA <- cos(rA * pi/180)

  res <- simpleRandomWalk(x[1], y[1], sinrA, cosrA, d)

  return(cbind(res[["rx"]], res[["ry"]]))
}

