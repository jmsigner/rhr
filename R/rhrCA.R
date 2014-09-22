## ============================================================================== ##  
## This program is free software: you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation, either version 3 of the License, or
## (at your option) any later version.
## 
## This program is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
## 
## You should have received a copy of the GNU General Public License
## along with this program.  If not, see <http://www.gnu.org/licenses/>.
## ============================================================================== ##

##' Estimate Core Area
##'
##' This function estimates core areas for animal home ranges. A core area, is an area of animals home range that is used more intensively.
##' 
##' @param x a RhrHREstimator* object
##' @param method method used to calculate core area. At the moment only powell90 is implemented
##' @param ... none implemented
##' @export
##' @return object of class RhrHRCoreArea
##' @note Core areas are areas within an animals home range that receive greater intensity of use than other areas. Core areas are often used to answer questions of territoriality or habitat selection. Oftentimes core areas are estimated by calculating home ranges at a level of 20 or 50%, i.e. determine a cut off value of the utilization distribution (Laver and Kelly 2008). This approach neglects differences by individuals. 
##' @note Seaman and Powell (1990) presented an area independent method for core area estimation, that does not rely on a predefined isopleth cut off values. This methods estimates the core area by estimating two values for each grid cell of the study area. First for each grid cell the number of relocations that fall within the cell are counted. In the next step the following two values are calculated for each cell:
##' \enumerate{
##' \item Percentage of maximum relative frequency. The grid cell with the highest relative frequency of relocations is assigned a value of 100 %
##' \item Percent of home range, that is the percentage of pixels with a higher relative frequency of relocations. The percentage of maximum relative frequency is then plotted against the percent of home range. The point with the greatest distance to the line with intercept 1 and slope -1 is used as threshold.
##' }

##' @references Peter N. Laver and Marcella J. Kelly. A critical review of home range studies. The Journal of Wildlife Management, 72(1):290-298, 2008
##' @references Erran D. Seaman and Roger A. Powell. Identifying patterns and intensity of home range use. Bears: their biology and management, 243-249, 1990


##' @rdname rhrCoreArea
##' @examples
##' \dontrun{
##' est <- rhrKDE(datSH[, 2:3])
##' ca <- rhrCoreArea(est)
##' plot(ca)
##' }

rhrCoreArea <- function(x, method="seaman90", ...) {
  UseMethod("rhrCoreArea", x)
}

##' @export
rhrCoreArea.default <- function (x , ...) {
  paste0("rhrCoreArea is not defined for object of class ", class(x))
}


##' @export
##' @rdname rhrCoreArea

rhrCoreArea.RhrEst <- function(x, method="powell90", ...) {

  if (!is(x, "RhrEst")) {
    stop("rhrCoreArea: x: not of class RhrHREstimator")
  }

  if (!rhrHasUD(x)) {
    stop("UD is required to calculate core area")
  }

  ## retrieves the ud
  r <- rhrUD(x)

  ## Standardize the ud to 1, not necessary anymore
  uds <- r[] / sum(r[], na.rm=TRUE)

  ## fraction of max uds
  fuds <- uds / max(uds, na.rm=TRUE)

  ## Create a rastre with fraction of maximum ud
  pctp.r <- setValues(r, fuds)

  ## Set projection
  proj4string(pctp.r) <- proj4string(r)

  ## oder fraction of maximum ud 
  fuds.o <- order(fuds, decreasing=TRUE)

  uds.fudso <- uds[fuds.o]

  pctp <- fuds[fuds.o]

  pctr <- rep(NA, length(uds))

  for (i in seq_along(uds)) {
    pctr[i] <- sum(uds.fudso >= uds.fudso[i], na.rm=TRUE) / length(uds.fudso)
  }
  dd <- sapply(1:length(pctr), function(i) distancePointLine(pctr[i], pctp[i], 0, 1, 1, 0))
  out <- list(pctprob=pctp, pctrange=pctr, rast=pctp.r >= pctp[which.max(dd)], dist=dd, method="powell")
  class(out) <- "RhrHRCoreArea"
  return(out)
}


distancePointLine <- function(x, y, x1, y1, x2, y2) {
  ## code from: http://en.wikipedia.org/wiki/Distance_from_a_point_to_a_line
  normalLength <- ((x2 - x1)^2 + (y2 - y1)^2)
  abs((x - x1) * (y2 - y1) - (y - y1) * (x2 - x1)) / normalLength
}


##' plot for RhrHRCoreArea
##' 
##' generic plot for RhrHREstimator
##' @param x RhrHRCoreArea object
##' @param ... none implemented
##' @method plot RhrHRCoreArea
##' @export

plot.RhrHRCoreArea <- function(x, ...) {
  ## Plot curve
  p <- ggplot(data.frame(x=x$pctrange, y=x$pctprob), aes(x=x, y=y)) +
    geom_point(alpha=0.04, position=position_jitter(height=0.01, width=0.01)) +
      xlim(c(0,1)) + ylim(c(0,1)) +
        geom_abline(intercept=1, slope=-1) + 
          geom_point(aes(x=x,y=y), colour="red", size=4,
                     data=data.frame(x=x$pctrange[which.max(x$dist)], y=x$pctprob[which.max(x$dist)])) +
                       geom_hline(aes(yintercept=y), colour="red", 
                                  data=data.frame(y=x$pctprob[which.max(x$dist)])) +
                                    theme_bw() + labs(title="Corea Area Estimation", x="Fraction of Home Range", y="Fraction of maximum Relative Frequency") + coord_fixed()
  return(p)
}

##' @export
rhrArea.RhrHRCoreArea <- function(x, ...) {
  tmp <- rhrIsopleths(x)
  ars <- tmp$area
  ars
}

##' @export
rhrIsopleths.RhrHRCoreArea <- function(x, ...) {
  con <- rasterToContour(x$rast, levels=1)

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

#### proj4string(con) <- projection(cud)  # set projection

  df <- data.frame(area=gArea(con, byid=TRUE))
  con <- SpatialPolygonsDataFrame(con, df)
}
