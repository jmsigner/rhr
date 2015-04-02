set.seed(123)
x <- rnorm(500)
y <- rnorm(500)
h <- seq(0.1, 3, 0.1)

context("rhrBaseLSCV")
test_that("Wrong input is recognized", {
  expect_error(rhrBaseLSCV(x, y))
  expect_error(rhrBaseLSCV(x, y[-1], h))
  expect_error(rhrBaseLSCV(x, as.character(y), h))
})

test_that("Dimensions of results are correct", {
  expect_equal(length(rhrBase::rhrBaseLSCV(x, y, h)), length(h))
})

context("rhrBaseMSD")
test_that("Wrong input is recognized", {
  expect_error(rhrBaseMSD(x))
  expect_error(rhrBaseMSD(x, y[-1]))
  expect_error(rhrBaseMSD(x, as.character(y)))
})

test_that("Dimensions of results are correct", {
  expect_equal(length(rhrBase::rhrBaseMSD(x, y)), 1)
})

context("rhrBasePRW")
test_that("Wrong input is recognized", {
  expect_error(rhrBasePRW(x))
  expect_error(rhrBasePRW(x, y[-1]))
  expect_error(rhrBasePRW(x, as.character(y)))
})

test_that("Dimensions of results are correct", {
  expect_equal(nrow(rhrBasePRW(x, y)), length(x))
})

context("rhrBase::rhrBaseIntervalSubset")
x <- seq(0, 3600, by = 60)

test_that("Wrong input is recognized", {
  expect_error(rhrBaseIntervalSubset(x))
  expect_error(rhrBaseIntervalSubset(x[1], 1))
  expect_error(rhrBaseIntervalSubset(x, as.character(y)))
})

test_that("Dimensions of results are correct", {
  expect_equal(length(rhrBaseIntervalSubset(x, 1)), length(x))
})
