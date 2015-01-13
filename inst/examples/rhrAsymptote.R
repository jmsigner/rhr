library(rhr)
data(datSH)

kde <- rhrKDE(datSH[, 2:3])
kdeA <- rhrAsymptote(kde)
plot(kdeA)

## This takes some time
kdeA <- rhrAsymptote(kde, ns = seq(10, nrow(datSH), 20), nrep = 20)
plot(kdeA)

## With MCP
mcp <- rhrMCP(datSH[, 2:3])
mcpA <- rhrAsymptote(mcp)
plot(mcpA)

## This takes some time
mcpA <- rhrAsymptote(mcp, ns = seq(10, nrow(datSH), 20), nrep = 20)
plot(mcpA)
