context("rhrHref: test cases")

set.seed(1234)
datSH <- data.frame(x=rnorm(500), y=rnorm(500))
Z <- complex(re=datSH[, "x"], im=datSH[, "y"])
datSp <- SpatialPoints(datSH[, c("x", "y")])


test_that("Wrong input is recognized", {
  expect_that(rhrHref(), throws_error())
  expect_that(rhrHref(1:10), throws_error())
  expect_that(rhrHref(abc), throws_error())
  expect_that(rhrHref(datSH, rescale="abc"), throws_error())
})

test_that("Correct class is returned", {
  expect_that(rhrHref(datSH), is_a("list"))
  expect_that(rhrHref(datSp), is_a("list"))
  expect_that(rhrHref(Z), is_a("list"))
  expect_that(rhrHref(datSH, rescale="none"), is_a("list"))
  expect_that(rhrHref(datSH, rescale="unitvar"), is_a("list"))
  expect_that(rhrHref(datSH, rescale="xvar"), is_a("list"))
  expect_that(rhrHref(datSH, rescale="none")$rescale, equals("none"))
  expect_that(rhrHref(datSH, rescale="unitvar")$rescale, equals("unitvar"))
  expect_that(rhrHref(datSH, rescale="xvar")$rescale, equals("xvar"))
})


