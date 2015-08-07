library(rhr)
library(lubridate)
data(datSH)

## Create a SpatialPoints objects with the relocation
sp <- sp::SpatialPoints(datSH[, 2:3])

## Parse time
time <- lubridate::ymd_hms(paste(datSH$day, datSH$time))

## Create an object of RhrTrackST (only space)
trackST <- rhrTrack(sp, time)

## What is the time span?
rhrTrackSpan(trackST)

## split the track by year
tracks <- rhrSplit(trackST, year(rhrTimes(trackST)))
tracks
