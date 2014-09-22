context("rhrKDE: test cases")

library(rhr)
data(datSH)


test_that("Test rhrMCP", {
  ## res
  resKDE <- rhr::rhrKDE(datSH[, 2:3])
  expect_that(resKDE, is_a("RhrKDE"))
  expect_that(resKDE, is_a("RhrEst"))
  expect_that(resKDE, is_a("list"))
  expect_that(resKDE$call, is_a("call"))
  expect_that(resKDE$args, is_a("list"))
  expect_that(resKDE$res, is_a("list"))
  expect_that(resKDE$res$hr, is_a("RasterLayer"))
                 
  expect_that(resKDE$args$xy, is_a("data.frame"))
  expect_that(ncol(resKDE$args$xy), equals(2))
  expect_that(resKDE$args$proj4string, equals(NA))
  expect_that(is.na(proj4string(resKDE$res$hr)), is_true())

  expect_that(rhrArea(resKDE), is_a("numeric"))
  expect_that(names(rhrArea(resKDE)), equals("95"))
  expect_that(rhrIsopleths(resKDE), is_a("SpatialPolygons"))

  expect_that(rhr::rhrKDE(), throws_error())
  expect_that(rhr::rhrKDE(datSH[, 2:4]), gives_warning())
  expect_that(rhr::rhrKDE(datSH[, 1:4]), throws_error())

})

test_that("Test .rhrKDE", {
  ## res
  trast=rhrRasterFromExt(rhrExtFromPoints(datSH[, 2:3], extendRange=0.2), nrow=100, res=NULL)

  resKDE <- rhr::.rhrKDE(datSH[, 2:3], h=c(100,100), trast=trast)
  expect_that(resKDE, is_a("RasterLayer"))

  expect_that(rhr::.rhrKDE(datSH[, 2:3]), throws_error())
  expect_that(rhr::.rhrKDE(datSH[, 1:4]), throws_error())
  expect_that(rhr::.rhrKDE(datSH), throws_error())
})

test_that("rhrCUD", {
  expect_that(rhrCUD(rhrKDE(datSH[, 2:3])), is_a("RasterLayer"))
})

test_that("rhrUD", {
  expect_that(rhrUD(rhrKDE(datSH[, 2:3])), is_a("RasterLayer"))
})

test_that("rhrIsopleths", {
  expect_that(rhrIsopleths(rhrKDE(datSH[, 2:3])), is_a("SpatialPolygons"))
})

test_that("rhrArea", {
  expect_that(rhrArea(rhrKDE(datSH[, 2:3])), is_a("numeric"))
  expect_that(names(rhrArea(rhrKDE(datSH[, 2:3]), levels=c(50, 95))), equals(c("50", "95")))
})
