context("rhrReadData: test cases")

## Generate Data
library(rhr)
data(datSH)

dat <- data.frame(lon=c(1, 2, 3), lat=c(1, 2, 3))
dat1 <- data.frame(lon=c(1, 1, 3), lat=c(1, 1, 3))
dat2 <- data.frame(lon=c(1, NA, 3), lat=c(1, NA, 3))
dat3 <- data.frame(lon=c(1, 1, NA, 3), lat=c(1, 1, NA, 3))

fields <- list(lon="lon", lat="lat")

test_that("Missing filename is recognised", {
  expect_that(rhrMapFields(), throws_error())
})

test_that("correct return is given", {
  expect_that(rhrMapFields(dat, fields), is_a("list"))
  expect_that(rhrMapFields(dat, fields), is_a("RhrMappedData"))
  expect_that(rhrMapFields(dat1, fields), is_a("list"))
  expect_that(rhrMapFields(dat1, fields), is_a("RhrMappedData"))
  expect_that(rhrMapFields(dat2, fields), is_a("list"))
  expect_that(rhrMapFields(dat2, fields), is_a("RhrMappedData"))
  expect_that(rhrMapFields(dat3, fields), is_a("list"))
  expect_that(rhrMapFields(dat3, fields), is_a("RhrMappedData"))
})


test_that("correct return is given", {
  expect_that(nrow(rhrMapFields(dat, fields)$dat), is_equivalent_to(3))
  expect_that(nrow(rhrMapFields(dat1, fields)$dat), is_equivalent_to(2))
  expect_that(nrow(rhrMapFields(dat2, fields)$dat), is_equivalent_to(2))
  expect_that(nrow(rhrMapFields(dat3, fields)$dat), is_equivalent_to(2))
})
