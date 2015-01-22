library(rhr)
data(datSH)

rhrHref(datSH[, 2:3])
rhrHrefScaled(datSH[, 2:3])

kde1 <- rhrKDE(datSH[, 2:3], h = rhrHref(datSH[, 2:3])$h)
kde2 <- rhrKDE(datSH[, 2:3], h = rhrHrefScaled(datSH[, 2:3])$h)
kde3 <- rhrKDE(datSH[, 2:3], h = rhrHrefScaled(datSH[, 2:3], numOfParts = 2)$h)
kde4 <- rhrKDE(datSH[, 2:3], h = rhrHrefScaled(datSH[, 2:3], numOfParts = 3)$h)

plot(kde1)
plot(rhrIsopleths(kde2), border = "blue", lwd = 2, add = TRUE)
plot(rhrIsopleths(kde3), border = "green", lwd = 2, add = TRUE)
plot(rhrIsopleths(kde4), border = "red", lwd = 2, add = TRUE)

## what where the actual values of h?
sapply(list(kde1, kde2, kde3, kde4), rhrTuningParameter, msg = TRUE)
