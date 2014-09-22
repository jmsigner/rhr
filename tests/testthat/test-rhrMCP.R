context("rhrMCP: test cases")

library(rhr)
data(datSH)
datSHsp <- SpatialPoints(datSH[, 2:3])

test_that("Test .rhrMCP", {
  ## res
  resMCP <- rhr::.rhrMCP(datSHsp, level=95)
  expect_that(resMCP, is_a("SpatialPolygons"))

  expect_that(rhr::.rhrMCP(datSH[, 2:3]), throws_error())
  expect_that(rhr::.rhrMCP(datSH[, 1:4]), throws_error())
  expect_that(rhr::.rhrMCP(datSH), throws_error())

})

test_that("Test rhrMCP", {
  ## res
  resMCP <- rhr::rhrMCP(datSH[, 2:3])
  expect_that(resMCP, is_a("RhrMCP"))
  expect_that(resMCP, is_a("RhrEst"))
  expect_that(resMCP, is_a("list"))
  expect_that(resMCP$call, is_a("call"))
  expect_that(resMCP$args, is_a("list"))
  expect_that(resMCP$res, is_a("list"))
  expect_that(resMCP$res$hr, is_a("SpatialPolygons"))

  expect_that(resMCP$args$level, equals(95))
  expect_that(resMCP$args$xy, is_a("data.frame"))
  expect_that(ncol(resMCP$args$xy), equals(2))
  expect_that(resMCP$args$proj4string, equals(NA))
  expect_that(is.na(proj4string(resMCP$res$hr)), is_true())

  expect_that(rhrArea(resMCP), is_a("numeric"))
  expect_that(names(rhrArea(resMCP)), equals("95"))
  expect_that(rhrIsopleths(resMCP), is_a("SpatialPolygons"))

  expect_that(rhr::rhrMCP(), throws_error())
  expect_that(rhr::rhrMCP(datSH[, 2:4]), gives_warning())
  expect_that(rhr::rhrMCP(datSH[, 1:4]), throws_error())

})
