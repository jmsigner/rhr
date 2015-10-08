library(testthat)
library(rhr)
library(sp)
data(datSH)

context("rhrSiteFidelity: test cases")

fields <- list(lon="x_epsg31467",
               lat="y_epsg31467",
               id="collar",
               date="day",
               time="time")
dateFormat <- "ymd"
timeFormat <- "hms"

dat1 <- datSH[, 2:3]
dat2 <- SpatialPoints(datSH[, 2:3])
dat3 <- SpatialPointsDataFrame(datSH[, 2:3], data=datSH)
dat4 <- SpatialPointsDataFrame(datSH[, 2:3], data=datSH, proj4string=CRS("+init=epsg:31467"))
dat5 <- complex(real=datSH[, 2], imaginary=datSH[, 3])
dat6 <- rhrMapFields(datSH, fields, dateFormat=dateFormat, timeFormat=timeFormat)
dat7 <- rhrMapFields(datSH, fields, dateFormat=dateFormat, timeFormat=timeFormat,
                     projString=CRS("+init=epsg:31467"))

dat <- list(dat1, dat2, dat3, dat4, dat5, dat6, dat7)

test_that("Test rhrSiteFidelity", {
  expect_is(rhrSiteFidelity(dat[[1]]), "RhrSiteFidelity")
  expect_error(rhrSiteFidelity(dat[[1]], n=0))
  expect_error(rhrSiteFidelity(dat[[1]], alpha=-0.2))
  expect_error(rhrSiteFidelity(dat[[1]], alpha=1.1))
  expect_equal(sum(sapply(lapply(dat, rhrSiteFidelity), class) == "RhrSiteFidelity"), 7)
})

