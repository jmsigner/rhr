library(rhr)
data(datSH)

## MCP from data.frame
mcp1 <- rhrMCP(datSH[, 2:3], levels=95)

## Calculate mcp at several levels
mcp2 <- rhrMCP(datSH[, 2:3], levels=c(50, 90, 95))

## Area at each isopleth level
rhrArea(mcp1)
rhrArea(mcp2)

## plot data
plot(mcp1)

## SptialPolygonsDataFrame of isopleth
plot(rhrIsopleths(mcp2))

## Data: not projected
datrhr1 <- rhrMapFields(datSH, fields=list(lon="x_epsg31467", lat="y_epsg31467"))

## Data: projected
datrhr2 <- rhrMapFields(datSH, fields=list(lon="x_epsg31467", lat="y_epsg31467"),
                    projString=sp::CRS("+init=epsg:31467"))

datSP1 <- sp::SpatialPoints(datSH[, 2:3])

mcp4 <- rhrMCP(datrhr1)
mcp5 <- rhrMCP(datrhr2)
mcp6 <- rhrMCP(datSP1)

sapply(list(mcp1, mcp4, mcp5, mcp6), rhrArea)
