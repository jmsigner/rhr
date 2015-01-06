rhrUD2CUD <- function(x, trast=NULL, ...) {
  v <- x[]
  v <- v / sum(v, na.rm=TRUE)
  udFromDat <- raster::setValues(x, v)

  v <- cumsum(v[order(-v)])[order(order(-v))]
  r2 <- raster::setValues(x, v) 
  return(r2)
}
