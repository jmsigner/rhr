library(testthat)
library(rhr)
data(datSH)

context("rhrMCP: test cases")

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

test_that("Test .rhrMCP", {
  ## should only work with SP*
  expect_error(.rhrMCP(dat1, levels=95))
  expect_is(.rhrMCP(dat3, levels=95), "SpatialPolygons")
  expect_is(.rhrMCP(dat4, levels=95), "SpatialPolygons")
  expect_error(.rhrMCP(dat5, levels=95))
  expect_error(.rhrMCP(dat6, levels=95))
  expect_error(.rhrMCP(dat7, levels=95))
})

test_that("MCP works", {
  ests <- lapply(dat, rhrMCP)
  expect_true(all(sapply(ests, class)[1, ] == "RhrMCP"))
  expect_equal(sum(sapply(ests, "[[", "exitStatus")), 0)
  expect_true(all(sapply(ests, function(x) class(x$call)) == "call"))
  expect_true(all(sapply(ests, function(x) class(x$args)) == "list"))
  expect_true(all(sapply(ests, function(x) class(x$res)) == "list"))
  expect_true(all(sapply(ests, function(x) inherits(x$res$hr, "SpatialPolygons"))))
  expect_true(all(sapply(ests, rhrLevels) == 95))
  expect_equal(sd(unlist(sapply(ests, rhrArea)[2, ])), 0)
  expect_equal(sum(sapply(lapply(ests, rhrIsopleths), is.projected), na.rm=TRUE), 2)
  expect_equal(sum(sapply(ests, function(x) tryCatch(rhrUD(x), warning=function(w) return(TRUE)))), 7)
  expect_equal(sum(sapply(ests, function(x) tryCatch(rhrCUD(x), warning=function(w) return(TRUE)))), 7)
})
