library(rhr)
data(datSH)

hr1 <- rhrMCP(datSH[, 2:3])
hr2 <- rhrMCP(datSH[, 2:3], levels = seq(1, 99, 2))
hr3 <- rhrKDE(datSH[, 2:3], level = c(50, 90))

rhrLevels(hr1)
rhrLevels(hr2)
rhrLevels(hr3)
