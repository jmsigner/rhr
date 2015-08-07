library(rhr)
data(datSH)

## Create a SpatialPoints objects with the relocation
sp <- sp::SpatialPoints(datSH[, 2:3])

## Parse time
time <- lubridate::ymd_hms(paste(datSH$day, datSH$time))

## Create an object of RhrTrackST (only space)
trackST <- rhrTrack(sp, time)
time <- rhrTimes(trackST)
