library(rhr)
data(datSH)

## Create a SpatialPoints objects with the relocation
sp <- sp::SpatialPoints(datSH[, 2:3])

## Create an object of RhrTrackS (only space)
trackS <- rhrTrack(sp)

is.regular(trackS)
