rhrCUD2Isopleths <- function(cud, levels=95, ...) {

  levels <- rhrCheckLevels(levels)

  con <- rasterToContour(cud * 100, levels=levels)

  b <- coordinates(con)
  
  ## make sure there are at least 2 points
  b <- lapply(b, function(x) Filter(function(x) nrow(x) > 2, x))

  ## Make spatial polyon
  ## Complete ring and create each Polygon
  con <- lapply(b, function(x) {
    if (length(x) == 1) {
      lapply(x, function(xx) Polygon(rbind(xx, xx[1,])[, 1:2], hole=FALSE))

    } else { 
      bb <- SpatialPolygons(lapply(seq(length(x)), function(i) Polygons(list(Polygon(rbind(x[[i]], x[[i]][1,])[, 1:2])), i)))
      if (any((tm <- gIntersects(bb, byid=T))[upper.tri(tm)])) {

        ## some polys intersect find out which and set as wholes
        pos <- expand.grid(b=1:length(bb), s=1:length(bb))
        holes <- rep(FALSE, length(bb))

        for (i in 1:nrow(pos)) {
          if (gContainsProperly(bb[pos[i,1]], bb[pos[i,2]])) {

            ## second poly is contained by the first
            holes[pos[i,2]] <- TRUE
          }
        }

        lapply(seq_along(x), function(i) Polygon(rbind(x[[i]], x[[i]][1,])[, 1:2], hole=holes[i]))


      } else {
        lapply(x, function(xx) Polygon(rbind(xx, xx[1,])[, 1:2], hole=FALSE))

      }
    }
  })

  ## Check holes, if more than 1 poly, make sp polygons, then check wholes
  ## create a list of Polygons for each level
  con <- lapply(seq_along(con), function(i) Polygons(con[[i]], i))
  con <- SpatialPolygons(con)

  ## Set proj4string
###### 
  proj4string(con) <- proj4string(cud)  

  df <- data.frame(level=levels, area=gArea(con, byid=TRUE))
  row.names(df) <- 1:length(levels)
  con <- SpatialPolygonsDataFrame(con, df)
  return(con)
}
