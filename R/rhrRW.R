#' Simulate a random walk in 2D. 
#' 
#' Individum moves in a planar coordinate system according to a simple random walk.
#' 
#' @param xy0 Numeric vector of length two. The initial coordinates.
#' @param n Numeric scalar. The number of steps.
#' @param sigma Numeric scalar or vector of length \code{n}. Controlls the step lengths. 
#' @param time Vector of \code{POSIXct} objects. Providing the time stamp of relocations. If \code{time} is missing, a sequence of time stamps generated starting at the current time in one minute time steps.
#'
#' @return Object of class RhrTrajST
#' @export
#'
 
rhrRW <- function(xy0 = c(0, 0), n = 1000, sigma = 1, time) {
  
  if (missing(time)) {
    time <- lubridate::floor_date(lubridate::now(), "minute") + 
                                    lubridate::minutes(0:(n-1))
  }
  
  xy <- data.frame(x = xy0[1] + cumsum(rnorm(n, 0, sigma)), 
                   y = xy0[2] + cumsum(rnorm(n, 0, sigma)))
  xy <- sp::SpatialPoints(xy)
  rhrTrack(xy, time = time)
  
}
