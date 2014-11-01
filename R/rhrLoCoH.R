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

##' Local Convex Hull (LoCoH)
##'
##' This function estimates an animals home range using Local Convex Hulls (LoCoH).
##'
##' @param xy data.frame with two columns. The first column contains x coordinates and the second column contains y coordinates.
##' @param type character value, one of k, r, a. Three methods to select which neighbours are used for local hulls are availabe: i) k-nearest neighbours; ii) neighbours within a radius r; iii) neighbours within a cummulative dististance a.
##' @param n numeric value, if type is k it is the number of neaerst neibhers, if type is r it is the radius that is searched and if type is a it is the cummulative distance to be used.
##' @param minPts numeric value, the minimum number of neighbours required.
##' @param autoN boolean, determine \code{n} automatically. This take precedence over n, but is set to \code{FALSE} by default.
##' @details Three different types available for determining the number of neighbors:
##' \itemize{
##'  \item{"k"}{uses the k nearest neighbours}
##'  \item{"r"}{uses all neighbours within a radius r}
##'  \item{"a"}{uses all neighbours that can be reached within a distance a. The distance to all points is calculated and then the cummulatively summed starting from the smallest until \code{a} is reached.}}.
##' \code{autoN} is the square root of all locations, if \code{type} is "k". If \code{type} is "a" then "n" will be the maximum distance between any two relocations. If \code{type} is "r" the distance of "n" will be 5% of the maximum distance between any two points.
##' @return object of class \code{RhrHREstimator}
##' @author Johannes Signer 
##' @references Getz, W. M., & Wilmers, C. C. (2004). A local nearest-neighbor convex-hull construction of home ranges and utilization distributions. _Ecography_, 27(4), 489-505.
##' @references Getz, W. M., Fortmann-Roe, S., Cross, P. C., Lyons, A. J., Ryan, S. J., & Wilmers, C. C. (2007). LoCoH: nonparameteric kernel methods for constructing home ranges and utilization distributions. _PloS one_, 2(2), e207.
##' @export
##' @examples
##' \dontrun{
##' data(datSH)
##' locoh <- rhrLoCoH(datSH[, 2:3], type="k", n=10)
##'
##' ## area at isopleths
##' rhrArea(locoh)
##'
##' ## get isopleths
##' iso <- isopleths(locoh)
##'
##' ## Which parameter were used?
##' parameters(locoh)
##' }

rhrLoCoH <- function(xy, type="k", n=10, levels=95, minPts=3, proj4string=NA, autoN=FALSE) {

  ## ============================================================================== ##  
  ## Start

  ## Capture input arguments
  args <- as.list(environment())
  call <- match.call()

  ## check input coordinates
  projString <- rhrProjString(xy, projString=proj4string)
  xy <- rhrCheckData(xy, returnSP=FALSE)

  # input checking
  # type:
  if (!type %in% c("a", "k", "r")) {
    stop("rhrLocoh: incorrect type")
  }

  n <- as.numeric(n)
  if (is.na(n)) {
    stop(paste("rhrLocoh: n should be numeric, not ", n))
  }
  
  ## Are levels between 1 and 100, remove duplicated, order and add 0
  levels <- rhrCheckLevels(levels)

  ## determine n automatically and overwrite
  if (autoN) {
    if (type == "k") {
      n <- round(sqrt(nrow(xy)))
    }
    if (type == "a") {
      n <- round(max(dist(xy)))
    }
    if (type == "r") {
      n <- round(max(dist(xy)) * 0.05)
    }
    
  }
  
  ## calculation
  bb <- tryCatch(
    expr=list(msg=NULL, exitStatus=0, res=.rhrLoCoH(xy=xy, type=type, n=n, minPts=minPts, level=levels)),
    error=function(e) list(msg=e, exitStatus=1))

  if (bb$exitStatus == 0) {
    proj4string(bb$res) <- projString

  }

  res <- structure(
    list(
    exitStatus=bb$exitStatu,
    msg=bb$msg,
    call=call,
    args=args,
    res=list(hr=bb$res, n=n)),
    class=c("RhrLoCoH", "RhrEst", "list"))
  return(invisible(res))

}

.rhrLoCoH <- function(xy, type, n, minPts=minPts, level=levels, allLevels=FALSE) {
  ## All levels is set to FALSE, it can only be changed by actually calling rhr:::.rhrLoCoH
  no <- 1:nrow(xy)
  if (type == "k") {
    if (n > nrow(xy)) {
      n <- nrow(xy)
      warning(paste0("Locoh, type k, n > number of points, set n to number of points (", n, ")"))
    }
    ## 1. calc dist
    ## 2. order by dist
    ## 3. take n nearest
    aa <- lapply(no, function(i) no[order(sqrt((xy[,1] - xy[i,1])^2 + (xy[,2] - xy[i,2])^2))][1:n])
  } else if (type == "r") {
    ## 1. calc dist
    ## 2. take all pts with dist <= n
    aa <- lapply(no, function(i) no[sqrt((xy[,1] - xy[i,1])^2 + (xy[,2] - xy[i,2])^2) <= n])
  } else if (type == "a") {
    # 1. calc dist
    # 2. order by dist
    # 3. take cum dist
    # 4. take points where cumist <= n
    aa <- lapply(no, function(i) {
                   di <- sqrt((xy[,1] - xy[i,1])^2 + (xy[,2] - xy[i,2])^2)
                   no[order(di)][cumsum(di[order(di)]) <= n]
            })
  }


  ## remove the ones with less than minPts pts
  aa <- aa[sapply(aa, length) >= minPts]

  xysp <- SpatialPointsDataFrame(xy[, 1:2], data=data.frame(id=1:nrow(xy)))

  zz <- lapply(aa, function(x) xysp[x, ])
  mcps <- lapply(zz, gConvexHull)

  mcpAreas <- sapply(mcps, gArea)

  mcpAreasOrder <- order(mcpAreas)

  ff <- zz[mcpAreasOrder]
  mm <- mcps[mcpAreasOrder]

  pp <- rep(NA_integer_, length(ff))
  seen <- c()
  ## this is still slow
  for (i in 1:length(ff)) {
    seen <- union(seen, ff[[i]]$id)
    pp[i] <- length(unique(seen))
  }

  qq <- list()
  qq[[1]] <- mm[[1]]
  pp <- pp/nrow(xy) * 100
  
  if (!allLevels) {
    wlevel <- sapply(level, function(l) which.min(abs(pp - l)))
    for (i in seq_along(wlevel)) {
      ## buffer is necessary, to overcome some topology errors if the polygon is quasi a line
      p1 <- lapply(1:wlevel[i], function(i) Polygon(mm[[i]]@polygons[[1]]@Polygons[[1]]@coords))
      ff <- SpatialPolygons(list(Polygons(p1, ID=1)))

      qq[[i]] <- gBuffer(gUnaryUnion(ff), width=0, id=i)
    }

    rr <- do.call(rbind, qq)
    areas <- sapply(qq, gArea)

    qq2 <- SpatialPolygonsDataFrame(rr, data=data.frame(level=round(pp[wlevel], 2),
                                        area=areas), match.ID=FALSE)
  } else {

    qq[[1]] <- mm[[1]]
    for (i in 2:length(mm)) {
      ## buffer is necessary, to overcome some topology errors if the polygon is quasi a line
      qq[[i]] <- gBuffer(gUnaryUnion(gUnion(qq[[i-1]], mm[[i]])), width=0, id=i)
    }
    
    rr <- do.call(rbind, qq)
    areas <- sapply(qq, gArea)

    qq2 <- SpatialPolygonsDataFrame(rr, data=data.frame(level=round(pp, 2),
                                          area=areas), match.ID=FALSE)
    qq2 <- qq2[!duplicated(cbind(qq2$level, qq2$area)), ]
  }
  
  qq2
}



##' @export
print.RhrLoCoH <- function(x, ...) {
  cat("* rhrHREstimatorLoCoH \n")
  cat("* ----------------- \n")
  cat(sprintf("* Observations (n) : %s\n", nrow(x$args$xy)))
  cat(sprintf("* Levels           : %s\n", x$args$levels))
}

##' @export
rhrIsopleths.RhrLoCoH <- function(x, ...) {
  ## Levels
  x$res$hr
}

##' @export
rhrArea.RhrLoCoH <- function(x, ...) {
  data.frame(rhrIsopleths(x, ...))
}

##' @export
plot.RhrLoCoH <- function(x, title=NULL, ...) {
  ## fortify poly
  tempol <- rhrIsopleths(x, ...)
  tempol@data$id <- rownames(tempol@data)
  tempolPoints <- try(fortify(tempol, region="id"))
  tempolDF <- merge(tempolPoints, tempol@data, by="id")

  points <- x$args$xy
  names(points)[1:2] <- c("lon", "lat")

  ggplot(tempolDF, aes(x=long, y=lat, group=group, color=factor(level))) + 
    geom_point(data=points, aes(x=lon, y=lat, group=NULL, color=NULL), alpha=0.1) +
      geom_path(size=3, alpha=0.4) + 
        labs(colour=("LoCoH levels"), x="lon", y="lat", title=title) +
          geom_path(size=0.2, colour="black") +
            scale_color_manual(values=terrain.colors(10)) + theme_bw() 
}
