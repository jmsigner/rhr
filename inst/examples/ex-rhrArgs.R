library(rhr)
data(datSH)

## Geometric estimator
mcp <- rhrMCP(datSH[, 2:3])
rhrArgs(mcp)
