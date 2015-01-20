library(rhr)
data(datSH)

head(rhrData(rhrMCP(datSH[, 2:3])))
str(rhrData(rhrMCP(datSH[, 2:3]), spatial = TRUE))
