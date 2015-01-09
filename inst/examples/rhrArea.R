library(rhr)
data(datSH)

## Geometric estimator
mcp <- rhrMCP(datSH[, 2:3])
rhrArea(mcp)

mcp <- rhrMCP(datSH[, 2:3], levels = seq(50, 95, 5))
rhrArea(mcp)

## Probabilistic estimator
kde <- rhrKDE(datSH[, 2:3])
rhrArea(kde)

## at a different isopleth level
rhrArea(kde, levels = 50)

## or at sequence of isopleth levels
rhrArea(kde, level = seq(50, 95, 5))
