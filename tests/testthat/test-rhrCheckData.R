context("rhrCheckData: test cases")

set.seed(1234)
datSH <- data.frame(x=rnorm(500), y=rnorm(500))
Z <- complex(re=datSH[, "x"], im=datSH[, "y"])
datSp <- SpatialPoints(datSH[, c("x", "y")])


test_that("Wrong input is recognized", {
  expect_that(rhr:::rhrCheckData(), throws_error())
  expect_that(rhr:::rhrCheckData(1:10), throws_error())
})

test_that("Correct class is returned", {
  expect_that(rhr:::rhrCheckData(Z), is_a("data.frame"))
  expect_that(rhr:::rhrCheckData(datSH[, c("x", "y")]), is_a("data.frame"))
  expect_that(rhr:::rhrCheckData(datSH[, c("x", "y", "x")]), gives_warning())
  expect_that(rhr:::rhrCheckData(datSp), is_a("data.frame"))
})


