library(rhr)

data(datSH)

ts <- lubridate::ymd(datSH$day) + lubridate::hms(datSH$time)
pts <- sp::SpatialPoints(datSH[, 2:3])

traj <- rhrTraj(pts, ts)
