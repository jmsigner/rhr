context("rhrProjectString: test cases")

library(rhr)
data(datSH)
dat <- datSH[1:100, 2:3]
datSP <- SpatialPoints(dat, proj4string=CRS("+init=epsg:31467"))


test_that("Correct class is returned", {
  expect_that(rhr:::rhrProjString(dat), equals(CRS(NA_character_)))
  expect_that(rhr:::rhrProjString(dat, "+init=epsg:31467"), equals(CRS("+init=epsg:31467")))
  expect_that(rhr:::rhrProjString(datSP, "+init=epsg:3035"), equals(CRS("+init=epsg:31467")))
  expect_that(rhr:::rhrProjString(datSP), equals(CRS("+init=epsg:31467")))

  expect_that(proj4string(rhrIsopleths(rhrMCP(datSP))), equals("+init=epsg:31467"))
  expect_that(proj4string(rhrIsopleths(rhrLoCoH(datSP))), equals("+init=epsg:31467"))
  expect_that(proj4string(rhrIsopleths(rhrKDE(datSP))), equals("+init=epsg:31467"))
  expect_that(proj4string(rhrUD(rhrKDE(datSP))), equals("+init=epsg:31467"))
  expect_that(proj4string(rhrCUD(rhrKDE(datSP))), equals("+init=epsg:31467"))
  expect_that(proj4string(rhrCoreArea(rhrKDE(datSP))$rast), equals("+init=epsg:31467"))

  expect_that(proj4string(rhrIsopleths(rhrMCP(dat))), equals(NA_character_))
  expect_that(proj4string(rhrIsopleths(rhrLoCoH(dat))), equals(NA_character_))
  expect_that(proj4string(rhrIsopleths(rhrKDE(dat))), equals(NA_character_))
  expect_that(proj4string(rhrUD(rhrKDE(dat))), equals(NA_character_))
  expect_that(proj4string(rhrCUD(rhrKDE(dat))), equals(NA_character_))
  expect_that(proj4string(rhrCoreArea(rhrKDE(dat))$rast), equals(NA_character_))

})

