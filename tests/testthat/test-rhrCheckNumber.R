context("rhrCheckNumber: test cases")

test_that("wrong input is recognized", {
  expect_that(rhrCheckNumber(), throws_error())
  expect_that(rhrCheckNumber("abc", "a"), throws_error())
})

test_that("Correct class is returned", {
  expect_that(rhrCheckNumber(12, "a", 1, 0, 100), equals(12))
  expect_that(rhrCheckNumber(12, "a", 3, 0, 100), equals(c(12, 12, 12)))
  expect_that(rhrCheckNumber(c(10, 12, 14), "a", 1, 0, 100), equals(10))

  expect_that(rhrCheckNumber(12, "a", 1, 15, 100), throws_error())
  expect_that(rhrCheckNumber(12, "a", 1, 1, 10), throws_error())
  expect_that(rhrCheckNumber(12, "a", 1, 1, 10), throws_error())
  expect_that(rhrCheckNumber(c(12, NA), "a", 1, 1, 100), throws_error())

})


