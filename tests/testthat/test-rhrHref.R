context("rhrHref: test cases")

set.seed(1234)
datSH <- data.frame(x=rnorm(500), y=rnorm(500))
Z <- complex(re=datSH[, "x"], im=datSH[, "y"])
datSp <- SpatialPoints(datSH[, c("x", "y")])


test_that("Wrong input is recognized", {
  expect_error(rhrHref())
  expect_error(rhrHref(1:10))
  expect_error(rhrHref(abc))
  expect_error(rhrHref(datSH, rescale="abc"))
})

test_that("Correct class is returned", {
  expect_is(rhrHref(datSH), "list")
  expect_is(rhrHref(datSp), "list")
  expect_is(rhrHref(Z), "list")
  expect_is(rhrHref(datSH, rescale="none"), "list")
  expect_is(rhrHref(datSH, rescale="unitvar"), "list")
  expect_is(rhrHref(datSH, rescale="xvar"), "list")
  expect_equal(rhrHref(datSH, rescale="none")$rescale, "none")
  expect_equal(rhrHref(datSH, rescale="unitvar")$rescale, "unitvar")
  expect_equal(rhrHref(datSH, rescale="xvar")$rescale, "xvar")
})


