library(testthat)
library(rhr)
library(lubridate)
library(sp)

data(datSH)

context("rhrBBMM: test cases")
datSH <- datSH[!duplicated(datSH[, 2:3]), ]
datSH <- datSH[1:100, ]
dat1 <- datSH
dat1$ts <- ymd(datSH$day) + hms(datSH$time)

test_that("BBMM works", {
#  ests <- rhrBBMM(dat1[, 1:2], dat1$ts)
})




