context("rhrLevels: test cases")
test_that("Wrong input is recognized", {
  expect_that(rhr:::rhrCheckLevels(), throws_error())
  expect_that(rhr:::rhrCheckLevels(c(1, NA, 3)), throws_error())
  expect_that(rhr:::rhrCheckLevels("A"), throws_error())
  expect_that(rhr:::rhrCheckLevels(1:101), throws_error())

  expect_that(rhr:::rhrCheckLevels(1:10), equals(1:10))
  expect_that(rhr:::rhrCheckLevels("10"), equals(10))

})
