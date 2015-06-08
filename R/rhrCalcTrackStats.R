rhrCalcTrackStats <- function(x, ...) {
  UseMethod("rhrCalcTrackStats")
}

rhrCalcTrackStats.RhrTrajSTR <- function(x, ...) {
  xx <- data.frame(coordinates(rhrTrajRelocations(x)))
  names(xx) <- c("x", "y")
  time <- time(rhrTrajLocations(x))
  dx <- diff(xx$x)
  dy <- diff(xx$y)
  len <- sqrt(dx^2 + dy^2)
  absAngle <- atan2(dy, dx)
  dA <- diff(absAngle)
  dA <- ifelse(abs(dA) > pi, sign(dA) * (abs(dA) - 2 * pi), dA)
  relAngle <- c(NA, dA)
  dt <- diff(as.numeric(time))
  start <- head(time, -1)
  missing <- diff(as.integer(time)) / x[["properties"]]$by - 1
  mTime <- apply(data.frame(time[-length(time)], time[-1]), 1, mean)
  data.frame(len, absAngle, relAngle, dt, start, missing)
}

rhrCalcTrackStats.RhrTrajST <- function(dat, ...) {
  xx <- data.frame(coordinates(rhrTrajRelocations(dat)))
  names(xx) <- c("x", "y")
  time <- time(rhrTrajLocations(dat))
  dx <- diff(xx$x)
  dy <- diff(xx$y)
  len <- sqrt(dx^2 + dy^2)
  absAngle <- atan2(dy, dx)
  dA <- diff(absAngle)
  dA <- ifelse(abs(dA) > pi, sign(dA) * (abs(dA) - 2 * pi), dA)
  relAngle <- c(NA, dA)
  dt <- diff(as.numeric(time))
  start <- head(time, -1)
  data.frame(len, absAngle, relAngle, dt, start)
}


rhrCalcTrackStats.RhrTrajS <- function(dat, ...) {
  xx <- data.frame(coordinates(rhrTrajRelocations(dat)))
  names(xx) <- c("x", "y")
  dx <- diff(xx$x)
  dy <- diff(xx$y)
  len <- sqrt(dx^2 + dy^2)
  absAngle <- atan2(dy, dx)
  dA <- diff(absAngle)
  dA <- ifelse(abs(dA) > pi, sign(dA) * (abs(dA) - 2 * pi), dA)
  relAngle <- c(NA, dA)
  data.frame(len, absAngle, relAngle)
}
