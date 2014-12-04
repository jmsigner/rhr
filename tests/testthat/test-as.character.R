context("as.character: test cases")

set.seed(1234)
r <- raster()


test_that("function works", {
  expect_that(as.character(r), is_a("character"))
})

