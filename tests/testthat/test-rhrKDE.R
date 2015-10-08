library(testthat)
library(rhr)
library(magrittr)
library(sp)
data(datSH)

context("rhrKDE: test cases")

fields <- list(lon="x_epsg31467",
               lat="y_epsg31467",
               id="collar",
               date="day",
               time="time")
dateFormat <- "ymd"
timeFormat <- "hms"

datSH <- datSH[!duplicated(datSH[, 2:3]), ]
dat1 <- datSH[, 2:3]
dat2 <- SpatialPoints(datSH[, 2:3])
dat3 <- SpatialPointsDataFrame(datSH[, 2:3], data=datSH)
dat4 <- SpatialPointsDataFrame(datSH[, 2:3], data=datSH, proj4string=CRS("+init=epsg:31467"))
dat5 <- complex(real=datSH[, 2], imaginary=datSH[, 3])
dat6 <- rhrMapFields(datSH, fields, dateFormat=dateFormat, timeFormat=timeFormat)
dat7 <- rhrMapFields(datSH, fields, dateFormat=dateFormat, timeFormat=timeFormat,
                     projString=CRS("+init=epsg:31467"))

dat <- list(dat1, dat2, dat3, dat4, dat5, dat6, dat7)
trast <- rhrRasterFromExt(rhrExtFromPoints(dat7, extendRange=0.3), nrow=100, res=NULL)
h <- rhrHref(dat1)$h

test_that("Test .rhrKDE", {
  ## should only work with SP*
  expect_error(.rhrKDE(dat1))
  expect_is(.rhrKDE(dat1, h, trast), "RasterLayer")
  expect_error(.rhrKDE(dat2, h, trast))
  expect_error(.rhrKDE(dat3, h, trast))
  expect_error(.rhrKDE(dat4, h, trast))
  expect_error(.rhrKDE(dat5, h, trast))
  expect_error(.rhrKDE(dat6, h, trast))
  expect_error(.rhrKDE(dat7, h, trast))
})

test_that("KDE works", {
  ests <- lapply(dat, rhrKDE)
  expect_true(all(sapply(ests, class)[1, ] == "RhrKDE"))
  expect_equal(sum(sapply(ests, "[[", "exitStatus")), 0)
  expect_true(all(sapply(ests, function(x) class(x$call)) == "call"))
  expect_true(all(sapply(ests, function(x) class(x$args)) == "list"))
  expect_true(all(sapply(ests, function(x) class(x$res)) == "list"))
  expect_true(all(sapply(ests, function(x) inherits(x$res$hr, "RasterLayer"))))
  expect_true(all((xx <- sapply(ests, function(x) rhrLevels(x))) == mean(xx)))
  expect_equal(sd(unlist(sapply(ests, rhrArea)[2, ])), 0)
  expect_equal(sum(sapply(lapply(ests, function(x) proj4string(x$res$hr)), is.na)), 5)
  expect_equal(sum(sapply(lapply(ests, function(x) proj4string(rhrUD(x))), is.na)), 5)
  expect_equal(sum(sapply(lapply(ests, function(x) proj4string(rhrCUD(x))), is.na)), 5)
  expect_equal(sum(sapply(lapply(ests, rhrIsopleths), is.projected), na.rm=TRUE), 2)
  expect_true(all(lapply(ests, rhrData) %>% sapply(., class) %>% unlist %>% "=="(., "data.frame")))
  expect_true(all(lapply(ests, rhrData, spatial=TRUE) %>% sapply(., is, "SpatialPoints")))
  expect_true(all(sapply(ests, rhrHasUD)))
})



