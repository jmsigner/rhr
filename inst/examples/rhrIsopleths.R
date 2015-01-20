library(rhr)
data(datSH)

mcp <- rhrMCP(datSH[, 2:3])
kde <- rhrKDE(datSH[, 2:3])

rhrIsopleths(mcp)
rhrIsopleths(kde)
rhrIsopleths(kde, levels = c(20, 30, 40))
