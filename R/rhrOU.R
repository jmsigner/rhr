#' Simulate movement with home ranging  behavior as an Ornstein-Uhlenbeck process
#'
#' Implementation of Ornstein-Uhlenbeck process using the Euler-Maruyama method. \code{dt} is always set to 1.
#'
#' @param n Numeric scalar. The number of time steps. 
#' @param A Numeric matrix with dimension 2*2. The matrix \code{A} controlls the pull back towards the home-range center (\code{mu}). 
#' @param xy0 Numeric vector. The start position. If \code{xy0} is missing it is set to \code{mu}. 
#' @param mu Numeric vector. The home-range center. 
#' @param time Vector of \code{POSIXct} objects. Providing the time stamp of relocations. If \code{time} is missing, a sequence of time stamps generated starting at the current time in one minute time steps.
#' @param sigma Numeric scalar. Controls the exent of the walk.
#' @references Fieberg, J. (2007). Kernel density estimators of home range: smoothing and the autocorrelation red herring. Ecology, 88(4), 1059-1066.
#' @return An object of class \code{RhrTrajST}. 
#' @export
#' 
rhrOU <- function(n = 1000, A = matrix(c(0.1, 0, 0, 0.1), 2), xy0, mu = c(0,0), time, sigma = 0.3) {
  if (missing(time)) {
    time <- lubridate::floor_date(lubridate::now(), "minute") + 
      lubridate::minutes(0:(n-1))
  }
  if (missing(xy0)) {
    xy0 <- mu
  } else {
    if (inherits(xy0, "RhrTrack")) {
      xy0 <- tail(sp::coordinates(rhrPoints(xy0)), 1)
    }
    
  }
  
  ## Euler-Maruyama-Verfahren
  dx <- diff(cumsum(rnorm(n+1, 0, 1)))  
  dy <- diff(cumsum(rnorm(n+1, 0, 1)))
  
  sigma <- matrix(c(sigma, 0, 0, sigma), ncol = 2)
  
  W <- cbind(dx, dy) %*% sigma ##! Fieberg 2007 used sigma^2 here, but for me it only worked using sigma
  xy <- matrix(nrow = n, ncol = 2)
  
  xy[1, ] <- xy0
  
  for (i in 2:n) {
    xy[i, ] <- xy[i-1, ] + as.vector(A %*% (mu - xy[i-1, ]) * 1 + W[i, ])
  }
  
  rhrTrack(sp::SpatialPoints(xy), time = time)
}



