set.seed(123)

library(rhr)

a <- SpatialPointsDataFrame(matrix(rnorm(200), 100), data.frame(a = 1:100))
id <- rep(1:4, each = 25)
ts <- lubridate::now() + lubridate::minutes(1:100)

b1 <- rhrTracks(a, id = id) # RhrTracksS
b2 <- rhrTracks(a, id = id, ts = sample(ts)) # RhrTracksST
b3 <- rhrTracks(a, id = id, ts = ts) # RhrTracksSTR

# Area of interest
bbx <- rgeos::gEnvelope(a[1:3, ])

# For a Track
c1 <- rhrWithin(b1[[1]], bbx)
c2 <- rhrWithin(b2[[1]], bbx)
c3 <- rhrWithin(b3[[1]], bbx)
lapply(list(c1, c2, c3), class)


# For several Tracks
f1 <- rhrWithin(b1, bbx)
f2 <- rhrWithin(b2, bbx)
f3 <- rhrWithin(b3, bbx)

# Note thate `c3` is now `RhrTracksST` not `RhrTracksSTR` any more. 
lapply(list(c1, c2, c3), class)

