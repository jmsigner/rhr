context("rhrArea: test cases")

library(rhr)
library(lubridate)

data(datSH)
dat <- datSH[1:100, ]
dat$ts <- ymd(dat$day) + hms(dat$time)
dat <- dat[, c(2, 3, 6)]

res <- list(rhrMCP(dat), rhrKDE(dat), rhrLoCoH(dat), rhrBiCirc(dat), rhrBiNorm(dat), rhrUniNorm(dat))



test_that("Test rhrArea", {
  ## res
  areas <- sapply(lapply(res, rhrArea), "[[", "area")
  expect_that(all(is.numeric(areas)), is_true())
})
