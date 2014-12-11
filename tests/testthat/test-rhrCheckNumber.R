context("rhrCheckNumber: test cases")

test_that("wrong input is recognized", {
  expect_error(rhrCheckNumber())
  expect_error(rhrCheckNumber("abc", "a"))
})

test_that("Correct class is returned", {
  expect_equal(rhrCheckNumber(12, "a", 1, 0, 100), 12)
  expect_equal(rhrCheckNumber(12, "a", 3, 0, 100), c(12, 12, 12))
  expect_equal(rhrCheckNumber(c(10, 12, 14), "a", 1, 0, 100), 10)

  expect_error(rhrCheckNumber(12, "a", 1, 15, 100)) 
  expect_error(rhrCheckNumber(12, "a", 1, 1, 10))
  expect_error(rhrCheckNumber(12, "a", 1, 1, 10))
  expect_error(rhrCheckNumber(c(12, NA), "a", 1, 1, 100))

})


