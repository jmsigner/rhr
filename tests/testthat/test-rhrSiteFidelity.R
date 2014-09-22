context("rhrSiteFidelity: test cases")

library(rhr)
data(datSH)
datSHsp <- SpatialPoints(datSH[, 2:3])

test_that("Test rhrSiteFidelity", {
  expect_that(rhrSiteFidelity(datSH), throws_error())
  expect_that(rhrSiteFidelity(datSH[, 2:3]), is_a("RhrSiteFidelity"))
  expect_that(rhrSiteFidelity(datSH[, 2:3]), is_a("list"))
  expect_that(rhrSiteFidelity(datSHsp), is_a("list"))
  expect_that(rhrSiteFidelity(datSH[, 2:3], n=-10), throws_error())
  expect_that(rhrSiteFidelity(datSH[, 2:3], n="10"), throws_error())
  expect_that(rhrSiteFidelity(datSH[, 2:3], alpha=2), throws_error())
})

