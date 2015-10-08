library(rhr)
data(datSH)

## KDE with default settings. h defaults to reference bandwidth
kde1 <- rhrKDE(datSH[, 2:3])
kde2 <- rhrKDE(datSH[, 2:3], h = rhrHpi(datSH[, 2:3])$h)
kde3 <- rhrKDE(datSH[, 2:3], h = rhrHrefScaled(datSH[, 2:3])$h)

par(mfrow = c(2, 2))
plot(kde1, main = "h = ref")
plot(kde2, main = "h = plug-in-the-equation")
plot(kde3, main = "h = ad hoc")
