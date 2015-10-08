library(rhr)
data(datSH)

## With KDE
kde <- rhrKDE(datSH[1:100, 2:3])
kdeA <- rhrAsymptote(kde)
plot(kdeA)


## With MCP
mcp <- rhrMCP(datSH[1:100, 2:3])
#mcpA <- rhrAsymptote(mcp)
#plot(mcpA)

