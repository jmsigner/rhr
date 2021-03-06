library(rhr)
data(datSH)

## Create a SpatialPoints objects with the relocation
sp <- sp::SpatialPoints(datSH[, 2:3])

## Parse time
time <- lubridate::ymd_hms(paste(datSH$day, datSH$time))

## Create an object of RhrTrackS (only space)
trackS <- rhrTrack(sp)
rhrN(trackS) 

## This the same as
nrow(trackS)

