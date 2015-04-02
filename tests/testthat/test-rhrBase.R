set.seed(123)
x <- rnorm(500)
y <- rnorm(500)
h <- seq(0.1, 3, 0.1)

context("rhrBaseLSCV")
test_that("Wrong input is recognized", {
  expect_error(rhr:::rhrBaseLSCV(x, y))
  expect_error(rhr:::rhrBaseLSCV(x, y[-1], h))
  expect_error(rhr:::rhrBaseLSCV(x, as.character(y), h))
})

test_that("Dimensions of results are correct", {
  expect_equal(length(rhr:::rhrBaseLSCV(x, y, h)), length(h))
})

context("rhrBaseMSD")
test_that("Wrong input is recognized", {
  expect_error(rhr:::rhrBaseMSD(x))
  expect_error(rhr:::rhrBaseMSD(x, y[-1]))
  expect_error(rhr:::rhrBaseMSD(x, as.character(y)))
})

test_that("Dimensions of results are correct", {
  expect_equal(length(rhr:::rhrBaseMSD(x, y)), 1)
})

context("rhrBasePRW")
test_that("Wrong input is recognized", {
  expect_error(rhr:::rhrBasePRW(x))
  expect_error(rhr:::rhrBasePRW(x, y[-1]))
  expect_error(rhr:::rhrBasePRW(x, as.character(y)))
})

test_that("Dimensions of results are correct", {
  expect_equal(nrow(rhr:::rhrBasePRW(x, y)), length(x))
})

context("rhrBase::rhrBaseIntervalSubset")
x <- seq(0, 3600, by = 60)

test_that("Wrong input is recognized", {
  expect_error(rhr:::rhrBaseIntervalSubset(x))
  expect_error(rhr:::rhrBaseIntervalSubset(x[1], 1))
  expect_error(rhr:::rhrBaseIntervalSubset(x, as.character(y)))
})

test_that("Dimensions of results are correct", {
  expect_equal(length(rhr:::rhrBaseIntervalSubset(x, 1)), length(x))
})
