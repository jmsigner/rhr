library(lubridate)
library(rhr)
data(datSH)

## Create a SpatialPoints objects with the relocation
sp <- sp::SpatialPoints(datSH[, 4:3])

## Parse time
time <- ymd_hms(paste(datSH$day, datSH$time))

## Create an object of RhrTrackS (only space)
trackS <- rhrTrack(sp)
class(trackS)

## Create an object of RhrTrackST (only space)
trackST <- rhrTrack(sp, time)
class(trackST)

## Create a regular trajectory
timeRounded <- round_date(time, "hour")
which6h <- which(hour(timeRounded) == 6)[10:40]
trackSTR <- rhrTrack(sp[which6h, ], timeRounded[which6h])
trackSTR

