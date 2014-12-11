library(rhr)
library(testthat)

context("rhrLevels: test cases")
test_that("Wrong input is recognized", {
 expect_error(rhr:::rhrCheckLevels())
 expect_error(rhr:::rhrCheckLevels(c(1, NA, 3)))
 expect_error(rhr:::rhrCheckLevels("A"))
 expect_error(rhr:::rhrCheckLevels(1:101))
 expect_equal(rhr:::rhrCheckLevels(1:10), 1:10)
 expect_equal(rhr:::rhrCheckLevels("10"), 10)
})
