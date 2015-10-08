library(lubridate)
library(rhr)
library(testthat)
library(rgeos)
library(sp)
data(datSH)

context("rhrTracks: test cases")
set.seed(112)
d <- SpatialPoints(data.frame(runif(1000), runif(1000)))

test_that("Test rhrTracks fails", {
  expect_error(rhrTracks(d, id = c(0, 0, sample(1:10, 999, TRUE))))
  expect_message(rhrTracks(d, id = c(0,  sample(1:10, 999, TRUE))))
})


set.seed(123)
a <- SpatialPointsDataFrame(matrix(rnorm(200), 100), data.frame(a = 1:100))
id <- rep(1:10, each = 10)
ts <- now() + minutes(1:100)
bbx <- gEnvelope(a[1:5, ])

b0 <- rhrTracks(a, id = sample(10, 100, TRUE))
b1 <- rhrTracks(a, id = id)
b2 <- rhrTracks(a, id = id, ts = sample(ts))
b3 <- rhrTracks(a, id = id, ts = ts)

c0 <- b0[[1]][1:4, ]
c1 <- b1[[1]][1:4, ]
c2 <- b2[[1]][1:4, ]
c3 <- b3[[1]][1:4, ]

d0 <- b0[[1]][c(1,3,4,5), ]
d1 <- b1[[1]][c(1,3,4,5), ]
d2 <- b2[[1]][c(1,3,4,5), ]
d3 <- b3[[1]][c(1,3,4,5), ]

e0 <- rhrWithin(b0[[1]], bbx)
e1 <- rhrWithin(b1[[1]], bbx)
e2 <- rhrWithin(b2[[1]], bbx)
e3 <- rhrWithin(b3[[1]], bbx)

f0 <- rhrWithin(b0, bbx)
f1 <- rhrWithin(b1, bbx)
f2 <- rhrWithin(b2, bbx)
f3 <- rhrWithin(b3, bbx)

test_that("Test rhrTracks classes are correct", {
  expect_is(b0, "RhrTracksS")
  expect_is(b0, "RhrTracks")
  expect_is(b0, "list")
  expect_is(b1, "RhrTracksS")
  expect_is(b1, "RhrTracks")
  expect_is(b1, "list")
  expect_is(b2, "RhrTracksST")
  expect_is(b2, "RhrTracksS")
  expect_is(b2, "RhrTracks")
  expect_is(b2, "list")
  expect_is(b3, "RhrTracksSTR")
  expect_is(b3, "RhrTracksST")
  expect_is(b3, "RhrTracksS")
  expect_is(b3, "RhrTracks")
  expect_is(b3, "list")
  
  expect_is(c0, "RhrTrackS")
  expect_is(c0, "RhrTrack")
  expect_is(c1, "RhrTrackS")
  expect_is(c1, "RhrTrack")
  expect_is(c2, "RhrTrackST")
  expect_is(c2, "RhrTrackS")
  expect_is(c2, "RhrTrack")
  expect_is(c3, "RhrTrackSTR")
  expect_is(c3, "RhrTrackST")
  expect_is(c3, "RhrTrackS")
  expect_is(c3, "RhrTrack")
  
  expect_is(d0, "RhrTrackS")
  expect_is(d0, "RhrTrack")
  expect_is(d1, "RhrTrackS")
  expect_is(d1, "RhrTrack")
  expect_is(d2, "RhrTrackST")
  expect_is(d2, "RhrTrackS")
  expect_is(d2, "RhrTrack")
  expect_is(d3, "RhrTrackST")
  expect_is(d3, "RhrTrackS")
  expect_is(d3, "RhrTrack")
  
  expect_is(e0, "RhrTrackS")
  expect_is(e0, "RhrTrack")
  expect_is(e1, "RhrTrackS")
  expect_is(e1, "RhrTrack")
  expect_is(e2, "RhrTrackST")
  expect_is(e2, "RhrTrackS")
  expect_is(e2, "RhrTrack")
  expect_is(e3, "RhrTrackST")
  expect_is(e3, "RhrTrackS")
  expect_is(e3, "RhrTrack")
  
  expect_is(f0, "RhrTracksS")
  expect_is(f0, "RhrTracks")
  expect_is(f0, "list")
  expect_is(f1, "RhrTracksS")
  expect_is(f1, "RhrTracks")
  expect_is(f1, "list")
  expect_is(f2, "RhrTracksST")
  expect_is(f2, "RhrTracksS")
  expect_is(f2, "RhrTracks")
  expect_is(f2, "list")
  expect_is(f3, "RhrTracksST")
  expect_is(f3, "RhrTracksS")
  expect_is(f3, "RhrTracks")
  expect_is(f3, "list")
})






